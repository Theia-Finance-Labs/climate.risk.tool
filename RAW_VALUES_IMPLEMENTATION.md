# Raw Intensity Values Implementation

## Problem
Flood depth values (and other hazard intensities) were being capped at the maximum value in the damage factors mapping file (600 cm for flood). Users need to see both:
- The **original extracted values** from the hazard indicators (uncapped)
- The **matched values** used for damage factor lookup (capped to mapping range)

## Solution
Created `[indicator]_raw` columns that preserve the original extracted intensity values before any matching/capping operations.

## Implementation

### 1. Raw Value Preservation (R/geospatial__join_damage_cost_factors.R)
Before `apply_intensity_matching()` is called, we now save raw values:

```r
# Save raw intensity values before matching/capping
if (length(intensity_cols) > 0) {
  for (intensity_col in intensity_cols) {
    if (intensity_col %in% names(base_table)) {
      raw_col_name <- paste0(intensity_col, "_raw")
      base_table[[raw_col_name]] <- base_table[[intensity_col]]
    }
  }
}

base_table <- apply_intensity_matching(base_table, mapping_df, intensity_cols, mapping$intensity_match)
```

This creates columns like:
- `flood_depth_cm_raw` - Original extracted flood depth
- `hi_raw` - Original extracted heat index
- `spi3_raw` - Original extracted drought index
- `fwi_raw` - Original extracted fire weather index

### 2. Prevent Rounding (R/mod_results_assets.R)
Modified the display formatting to skip rounding for `_raw` columns:

```r
for (col in numeric_col_names) {
  # Skip rounding for _raw columns to preserve exact extracted values
  if (grepl("_raw$", col)) {
    next
  }
  # ... rest of rounding logic
}
```

## Result

Users now see in the asset results CSV:
- `flood_depth_cm`: 600 (capped value used for damage factor lookup)
- `flood_depth_cm_raw`: 588.6715 (actual extracted value)

This allows:
1. **Correct calculations**: Damage factors use the capped value (600)
2. **Full transparency**: Users can see the actual hazard intensity (588.6715)
3. **Data analysis**: Users can analyze the distribution of raw hazard values

## Files Modified
1. `R/geospatial__join_damage_cost_factors.R` - Added raw value preservation
2. `R/mod_results_assets.R` - Skip rounding for _raw columns

## Testing
The `_raw` columns will appear in:
- Asset results CSV downloads (`asset_results_YYYY-MM-DD.csv`)
- Asset results Excel downloads (`asset_results_YYYY-MM-DD.xlsx`)
- All asset analysis tables in the UI
