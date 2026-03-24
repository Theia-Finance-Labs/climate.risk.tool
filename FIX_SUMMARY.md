# Fix Summary: Heat Hazard Mapping Issues

## Problem

Portfolio expected loss % change dropped from +5.03% (close to main's +5.00%) to +3.10% due to missing damage factors for Heat hazards on certain assets.

## Root Cause

Assets without CNAE (sector classification) codes could not be matched in the Heat mapping chain:

1. `cnae_exposure` mapping requires `cnae` → produces `lp_exposure`
2. `exposure_links` mapping requires `lp_exposure` → produces `damage_factor`

Without CNAE → no `lp_exposure` → no `damage_factor` → **zero heat losses**

### Affected Assets
- Company 4_Asset1 (commercial building)
- Company 4_Asset2 (commercial building)
- Company 3_Asset10 (agriculture)

## Solution

### 1. Data Fix (Primary)
Updated `workspace/demo_inputs_refacto/user_input/asset_information.xlsx`:
- Commercial/Industrial buildings → CNAE 68 (Real estate activities)
- Agriculture → CNAE 1 (Crop and animal production)

### 2. Code Enhancement (Defensive)
Added `defaults` support to mapping configuration:

**Heat.yml:**
```yaml
mappings:
  cnae_exposure:
    file: cnae_labor_productivity_exposure.csv
    variables: [lp_exposure]
    join:
      on_assets: [cnae]
    defaults:
      lp_exposure: median  # Fallback for assets without CNAE
```

**join_damage_cost_factors.R:**
Added logic to apply defaults for NA values after left join, with informative logging.

## Files Modified

### Code Changes (Git Tracked)
1. `R/geospatial__join_damage_cost_factors.R` - Added defaults support after mapping joins

### Configuration Changes (Workspace - Not Git Tracked)
2. `workspace/demo_inputs_refacto/hazards/config/Heat.yml` - Added defaults section
3. `workspace/demo_inputs_refacto/user_input/asset_information.xlsx` - Added CNAE codes to 3 assets

**Note**: Workspace files are in .gitignore. Changes to Heat.yml and asset_information.xlsx must be applied manually in production environments.

## Testing

To verify the fix works:
1. Run the app with the updated portfolio
2. Check that Company 4_Asset1, Company 4_Asset2, and Company 3_Asset10 now have:
   - `lp_exposure` values (not NA)
   - `damage_factor` values (not NA)
   - Non-zero Heat revenue/profit losses
3. Compare portfolio expected loss % change - should be closer to +5.03%

## Notes

- AGRI2 still shows discrepancies but user confirmed to ignore (out of raster bounds)
- Share of economic activity changes are expected and can be ignored
- The `defaults` feature provides a safety net for future missing mappings
