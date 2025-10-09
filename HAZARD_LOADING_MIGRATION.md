# Hazard Loading System Migration

## Overview
The hazard loading system has been completely redesigned to use a metadata-driven approach with a CSV mapping file.

## New Structure

### 1. Hazards Name Mapping File
Location: `{base_dir}/hazards_name_mapping.csv`

Required columns:
- `hazard_file`: Filename of the hazard raster (e.g., `global_pc_h10glob.tif`)
- `hazard_type`: Type of hazard (e.g., `flood`, `heat`)
- `scenario_code`: Short scenario code (e.g., `pc`, `rcp85`)
- `scenario_name`: Human-readable scenario name (e.g., `CurrentClimate`, `RCP8.5`)
- `hazard_return_period`: Return period in years (e.g., `10`, `100`, `1000`)

### 2. Hazards Directory
Location: `{base_dir}/hazards_world/`

Contains subdirectories by hazard type (e.g., `flood/`) with the actual `.tif` raster files.

## Key Functions & Workflow

The workflow uses THREE functions in sequence:

### 1. `read_hazards_mapping(mapping_path)`
- Reads the CSV mapping file ONCE
- Returns: tibble with columns: `hazard_file`, `hazard_type`, `scenario_code`, `scenario_name`, `hazard_return_period`

### 2. `load_hazards_from_mapping(mapping_df, hazards_dir, aggregate_factor = 1L)`
- Takes the mapping dataframe from step 1
- Loads the actual raster files
- Validates all files exist
- Checks for duplicates on filter columns
- Returns: Named list of SpatRaster objects

### 3. `list_hazard_inventory_from_metadata(mapping_df)`
- Takes the SAME mapping dataframe from step 1
- Creates UI-ready inventory
- Returns: tibble with `hazard_type`, `scenario_name`, `hazard_return_period`, `scenario_code`, `hazard_name`

**Key Point**: The mapping is read ONCE and used by BOTH functions 2 and 3

## UI Changes

### Hazard Selection (mod_hazards_events)
Now uses **three cascading dropdowns**:
1. **Hazard Type**: Select the type of hazard (flood, heat, etc.)
2. **Scenario**: Select the climate scenario (CurrentClimate, RCP8.5, etc.)
3. **Return Period**: Select the return period in years (10, 100, 1000)

The dropdowns cascade: selecting a hazard type filters the available scenarios, and selecting a scenario filters the available return periods.

## Configuration

### App (Production)
- Uses `aggregate_factor = 1` (no aggregation)
- Loads full-resolution rasters

### Tests
- Uses `aggregate_factor = 16` (pre-aggregated)
- Loads pre-aggregated files like `global_pc_h10glob__agg16.tif` for speed

## Migration Notes

### Removed
- ❌ Old `load_hazards()` direct directory scanning
- ❌ Backward compatibility code
- ❌ Old two-dropdown UI (hazard type + hazard name)

### Added
- ✅ `load_hazards_from_mapping()` - New mapping-based loader
- ✅ `list_hazard_inventory_from_metadata()` - Metadata-driven inventory
- ✅ Three-dropdown cascading UI (type → scenario → return period)
- ✅ Validation for file existence and duplicates
- ✅ Clear separation between aggregation factor and return period

## File Naming Convention

Hazard files follow this pattern:
```
global_{scenario_code}_h{return_period}glob.tif
```

Examples:
- `global_pc_h10glob.tif` - Current climate, 10-year return period
- `global_rcp85_h100glob.tif` - RCP8.5 scenario, 100-year return period

Pre-aggregated files add suffix:
- `global_pc_h10glob__agg16.tif` - Aggregated by factor 16

## Required Setup

1. Create `hazards_name_mapping.csv` in your base directory
2. Ensure all files listed in the CSV exist in `hazards_world/`
3. For tests: pre-aggregate files at factor 16 for speed
4. For production: use original files (factor 1)

## Benefits

1. **Clear metadata**: All hazard properties defined in one place
2. **Flexible filtering**: Easy to filter by type, scenario, and return period
3. **Validation**: Catches missing files and duplicates upfront
4. **Better UX**: Cascading dropdowns guide users through valid combinations
5. **No confusion**: Return period is clearly separate from aggregation factor

