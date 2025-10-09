# Migration to Precomputed Hazards

## Summary

The codebase has been updated to use precomputed hazard statistics for municipalities and provinces, eliminating the need to load GeoJSON boundary files and perform spatial matching for assets without coordinates.

## What Changed

### New Functions
- `read_precomputed_hazards(base_dir)` - Reads `precomputed_adm_hazards.csv` containing hazard statistics aggregated at ADM1 (province) and ADM2 (municipality) levels
- `create_asset_geometries(assets_df, ...)` - Simplified function for coordinate-based geolocation only

### Modified Functions
- `extract_hazard_statistics(assets_df, hazards, precomputed_hazards, ...)` - Now automatically handles:
  - Assets WITH coordinates → Spatial extraction from rasters
  - Assets WITHOUT coordinates but WITH municipality → Lookup from precomputed ADM2 data
  - Assets WITHOUT coordinates/municipality but WITH province → Lookup from precomputed ADM1 data
  
- `compute_risk(assets, companies, events, hazards, precomputed_hazards, ...)` - Updated signature:
  - OLD: `areas` parameter (list with municipalities and provinces)
  - NEW: `precomputed_hazards` parameter (data frame from `read_precomputed_hazards()`)

### Deprecated (but not removed)
- `geolocate_assets()` - Replaced by `create_asset_geometries()` + precomputed lookups
- `load_municipalities()`, `load_provinces()`, `load_location_areas()` - No longer needed for main workflow

## How to Update Your Code

### Before
```r
# Old approach
hazards <- load_hazards(...)
areas <- load_location_areas(
  file.path(base_dir, "areas", "municipality"),
  file.path(base_dir, "areas", "province")
)

results <- compute_risk(
  assets = assets,
  companies = companies,
  events = events,
  hazards = hazards,
  areas = areas,  # OLD
  damage_factors = damage_factors
)
```

### After
```r
# New approach
hazards <- load_hazards(...)
precomputed_hazards <- read_precomputed_hazards(base_dir)

results <- compute_risk(
  assets = assets,
  companies = companies,
  events = events,
  hazards = hazards,
  precomputed_hazards = precomputed_hazards,  # NEW
  damage_factors = damage_factors
)
```

## Benefits

1. **Performance**: No need to load large GeoJSON files or perform spatial joins
2. **Simplicity**: Single CSV file instead of multiple geospatial files
3. **Consistency**: Municipality/province assets get consistent hazard values from precomputed data
4. **Flexibility**: Still supports coordinate-based spatial extraction when available

## Test Files Needing Update

The following test files still use the old `areas` pattern and should be updated:

1. `tests/testthat/test-compute_risk.R`
2. `tests/testthat/test-compute_risk__hazard_filtering.R`

**Find/Replace Pattern:**
- Find: `areas <- load_location_areas(...)`
- Replace with: `precomputed_hazards <- read_precomputed_hazards(base_dir)`

- Find: `areas = areas,`
- Replace with: `precomputed_hazards = precomputed_hazards,`

## Data Requirements

Ensure `precomputed_adm_hazards.csv` exists in your base_dir with columns:
- region (municipality or province name)
- adm_level ("ADM1" for province, "ADM2" for municipality)
- hazard_type, hazard_return, scenario_code, scenario_name
- Statistics: min, max, mean, median, p2_5, p5, p95, p97_5

## Backward Compatibility

Old functions (`geolocate_assets`, `load_municipalities`, etc.) remain in the codebase and can still be used if needed, but are not required for the main workflow.

