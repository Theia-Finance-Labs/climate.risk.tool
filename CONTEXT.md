# Climate Risk Tool - AI Agent Context

## Project Overview

R package built with {golem} framework for Shiny apps. Performs climate risk analysis on assets and companies using geospatial hazard data.

## Core Architecture

### Data Pipeline

The tool processes climate risk through a 16-step pipeline orchestrated by `compute_risk()`:

1. **Input Loading**: Assets, companies, hazards, precomputed hazards, damage factors
2. **Asset Filtering**: Filter assets to only include those with matching companies
3. **Geospatial Processing**: Assign hazard values to assets using priority cascade
4. **Hazard-Damage Mapping**: Join damage cost factors based on hazard intensity
5. **Baseline Trajectories**: Compute revenue and profit projections
6. **Shock Application**: Apply acute and chronic event shocks
7. **Scenario Building**: Combine baseline and shock trajectories
8. **Discounting**: Apply present value calculations
9. **Company Aggregation**: Roll up asset-level to company-level metrics
10. **Risk Metrics**: Compute NPV, PD (Merton), Expected Loss
11. **Result Formatting**: Pivot to wide format for reporting

### Hazard Assignment Priority Cascade

Assets are assigned hazard values using this priority:
1. **Coordinates** (lat/lon) → Spatial extraction from raster files
2. **Municipality** (ADM2) → Precomputed lookup from `precomputed_adm_hazards.csv`
3. **Province** (ADM1) → Precomputed lookup from `precomputed_adm_hazards.csv`
4. **None** → Error with informative message

This is handled automatically by `extract_hazard_statistics()`.

## Data Requirements

### Directory Structure

```
{base_dir}/
├── user_input/
│   ├── asset_information.xlsx
│   └── company.xlsx
├── damage_and_cost_factors.csv
├── precomputed_adm_hazards.csv
├── hazards_name_mapping.csv
└── hazards_world/
    ├── flood/
    │   ├── global_pc_h10glob.tif
    │   ├── global_rcp85_h100glob.tif
    │   └── ...
    ├── heat/
    └── ...
```

### Required Input Files

#### 1. `asset_information.xlsx`
Columns: asset_id, company_id, asset_category, size_in_m2, location info (lat/lon OR municipality OR province)

#### 2. `company.xlsx`
Columns: company_id, company_name, equity, debt, other financial data

#### 3. `damage_and_cost_factors.csv`
Columns: hazard_type, hazard_intensity (rounded), asset_category, damage_share, cost_factor

#### 4. `precomputed_adm_hazards.csv`
Columns: region, adm_level (ADM1/ADM2), hazard_type, scenario_code, scenario_name, hazard_return, min, max, mean, median, p2_5, p5, p95, p97_5

Pre-aggregated hazard statistics for administrative regions. Eliminates need for GeoJSON boundary files.

#### 5. `hazards_name_mapping.csv`
Columns: hazard_file, hazard_type, scenario_code, scenario_name, hazard_return_period

Maps physical hazard files to metadata for UI display and filtering.

### Hazard Data Files

The tool supports three hazard data formats:

#### GeoTIFF Files (.tif)
Location: `{base_dir}/hazards/{hazard_type}/`

Naming convention: `global_{scenario_code}_h{return_period}glob.tif`

Examples:
- `global_pc_h10glob.tif` - Current climate, 10-year return period
- `global_rcp85_h100glob.tif` - RCP8.5, 100-year return period

**Metadata:** Defined in `hazards_metadata.csv` (hazard_file, hazard_type, scenario_code, scenario_name, hazard_return_period)

**Extraction:** Polygon-based (crop/mask with aggregation function)

#### NetCDF Files (.nc)
Location: `{base_dir}/hazards/{hazard_type}/{hazard_indicator}/{model_type}/{file}.nc`

Examples:
- `hazards/Drought/CDD/ensemble/ensemble_return_period.nc`
- `hazards/Compound/FWI/ensemble/ensemble_return_periods.nc`

**Metadata:** Extracted from folder structure and NC file contents
- `hazard_type`: From path (e.g., "Drought", "Compound")
- `hazard_indicator`: From path (e.g., "CDD", "FWI")
- `GWL` (Global Warming Level): From NC dimensions (e.g., "present", "1.5", "2", "3")
- `return_period`: From NC dimensions (e.g., 5, 10, 25, 50, 100)
- `ensemble`: Only 'mean' ensemble loaded by default

**Georeferencing:** NC files store lat/lon as cell centers. Loader calculates resolution and extends extent by half-pixel to create proper raster edges.

**Extraction:** Polygon-based (crop/mask with aggregation function)

**NC Ensemble Handling:** Only 'mean' ensemble is loaded as a `SpatRaster` with naming convention: `{hazard_type}__{hazard_indicator}__GWL={level}__RP={period}__ensemble=mean`. This provides representative values without loading all ensemble variants.

#### CSV Files (.csv)
Location: `{base_dir}/hazards/{hazard_type}/{hazard_indicator}/{model_type}/{file}.csv`

Examples:
- `hazards/Compound/HI/ensemble/ensemble_return_period.csv`

**Required columns:** `ensemble`, `GWL`, `return_period`, `lat`, `lon`, `hazard_indicator`, `hazard_intensity`

**Data structure:** Point-based format where each row represents a geolocated point with hazard values

**Metadata:** Extracted from folder structure and CSV columns
- `hazard_type`: From path (e.g., "Compound")
- `hazard_indicator`: From path (e.g., "HI")
- `GWL`: From CSV column (e.g., "present", "1.5", "2")
- `return_period`: From CSV column (e.g., 5, 10, 25, 50, 100)
- `ensemble`: Only 'mean' ensemble loaded by default

**Extraction:** Closest-point assignment (Euclidean distance in lat/lon coordinates)

**CSV Naming Convention:** `{hazard_type}__{hazard_indicator}__GWL={gwl}__RP={rp}__ensemble=mean`

**Mixed Type Validation:** Each leaf directory (e.g., `hazards/Compound/HI/ensemble/`) must contain only ONE file type (.tif, .nc, or .csv). Mixed types in the same folder will raise an error.

## Key Functions

### Main Orchestrator

**`compute_risk(assets, companies, events, hazards, precomputed_hazards, damage_factors, growth_rate, net_profit_margin, discount_rate)`**
- Returns: `list(assets, companies, assets_yearly, companies_yearly)`
- Orchestrates entire pipeline from raw inputs to final risk metrics
- Filters assets to only those with matching companies
- Uses priority cascade for hazard assignment

### Data Loading

**`read_assets(base_dir)`** → data.frame
- Reads from `{base_dir}/user_input/asset_information.xlsx`

**`read_companies(file_path)`** → data.frame
- Reads company data from specified Excel path

**`read_damage_cost_factors(base_dir)`** → data.frame
- Reads from `{base_dir}/damage_and_cost_factors.csv`

**`read_precomputed_hazards(base_dir)`** → data.frame
- Reads from `{base_dir}/precomputed_adm_hazards.csv`


### Hazard Loading Workflow

**1. `load_hazards_and_inventory(hazards_dir, aggregate_factor = 1L)`** → list(hazards, inventory)
- **Unified loader** for TIF, NetCDF, and CSV files
- Validates no mixed file types (.tif/.nc/.csv) in same leaf directory
- Scans for TIF mapping file (`hazards_metadata.csv`); if absent, skips TIF loading
- Auto-discovers NC files by scanning directory tree
- Auto-discovers CSV files by scanning directory tree
- Returns: `list(hazards = list(tif = ..., nc = ..., csv = ...), inventory = tibble(...))`
- **TIF**: Loads from `hazards_metadata.csv` as SpatRaster objects
- **NC**: Auto-discovers files, parses dimensions, creates one SpatRaster per (GWL × return_period) combination
  - Only 'mean' ensemble loaded by default
  - Naming: `{type}__{indicator}__GWL={level}__RP={period}__ensemble=mean`
- **CSV**: Auto-discovers files, reads point data as data frames
  - Only 'mean' ensemble loaded by default
  - Naming: `{type}__{indicator}__GWL={level}__RP={period}__ensemble=mean`
- **Inventory**: Combined metadata tibble with `source` column ("tif", "nc", or "csv")

**Application Usage:**
```r
# In mod_control_server:
hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
# Access hazards (flattened for compute pipeline):
hazards_flat <- c(hazard_data$hazards$tif, hazard_data$hazards$nc, hazard_data$hazards$csv)
# Access inventory (for UI dropdowns):
inventory <- hazard_data$inventory
```

**Naming Convention:**
- TIF: `{hazard_type}__{scenario_code}_h{return_period}glob` (e.g., `flood__pc_h10glob`)
- NC: `{hazard_type}__{indicator}__GWL={gwl}__RP={rp}__ensemble=mean` (e.g., `Drought__CDD__GWL=present__RP=10__ensemble=mean`)
- CSV: `{hazard_type}__{indicator}__GWL={gwl}__RP={rp}__ensemble=mean` (e.g., `Compound__HI__GWL=present__RP=5__ensemble=mean`)

### Geospatial Processing

**`create_asset_geometries(assets_df, default_buffer_size_m, output_crs)`** → sf object
- Creates point geometries with buffers for assets WITH coordinates only
- Uses `size_in_m2` for buffer sizing
- Raises error if coordinates missing

**`extract_hazard_statistics(assets_df, hazards, hazards_inventory, precomputed_hazards, events)`** → long format data.frame
- **Main orchestrator** that dispatches to specialized extraction functions:
  - **Coordinate-based assets** → `extract_spatial_statistics()` for spatial extraction (TIF/NC/CSV)
  - **Administrative-based assets** → `extract_precomputed_statistics()` for lookup
- **Priority cascade** for asset location:
  1. Coordinates → spatial extraction (polygon-based for TIF/NC, closest-point for CSV)
  2. No coordinates + municipality → precomputed ADM2 lookup
  3. No coordinates + province → precomputed ADM1 lookup
  4. None → Error
- Returns long format with columns: `hazard_intensity`, `matching_method`, etc.
- Includes diagnostic logging to show asset routing and matching method summary

**`extract_spatial_statistics(assets_df, hazards, hazards_inventory, aggregation_method)`** → long format data.frame (internal)
- Routes to appropriate extraction method based on hazard source:
  - **CSV hazards** → `extract_csv_statistics()` for closest-point assignment
  - **TIF/NC hazards** → Polygon-based extraction (crop, mask, aggregate)
- Used for assets WITH coordinates
- Returns `matching_method = "coordinates"`
- Adds `__extraction_method={aggregation_method}` suffix to hazard names

**`extract_csv_statistics(assets_df, hazards_csv, hazards_inventory, aggregation_method)`** → long format data.frame (internal)
- Closest-point assignment for CSV point data
- For each asset, calculates Euclidean distance to all CSV points: `sqrt((lat_asset - lat_csv)^2 + (lon_asset - lon_csv)^2)`
- Assigns hazard_intensity from nearest point
- Used for assets WITH coordinates and CSV hazards
- Returns `matching_method = "coordinates"`
- Adds `__extraction_method={aggregation_method}` suffix for consistency (though method doesn't affect closest-point selection)

**`extract_precomputed_statistics(assets_df, precomputed_hazards, hazards_inventory, events)`** → long format data.frame (internal)
- Lookup from precomputed administrative hazard data
- Used for assets WITHOUT coordinates
- Priority: municipality (ADM2) > province (ADM1)
- Validates required hazards from events against available precomputed data
- Returns `matching_method = "municipality"` or `"province"`
- Raises detailed errors if region or hazard combo not found

**`join_damage_cost_factors(assets_long_format, damage_factors_df)`** → data.frame
- Joins on hazard_type, rounded hazard_intensity, asset_category

### Financial Calculations

**`filter_assets_by_companies(assets, companies)`** → filtered assets
- Filters assets to only include those with companies in companies data

**`compute_baseline_trajectories(baseline_assets, companies, growth_rate, net_profit_margin)`** → yearly baseline
- Computes baseline revenue and profit trajectories over time

**`compute_shock_trajectories(yearly_baseline, assets_with_factors, events)`** → shocked yearly
- Splits events into acute/chronic
- Applies shocks sequentially (acute first, then chronic)

**`concatenate_baseline_and_shock(baseline_yearly, shocked_yearly)`** → combined scenarios
- Concatenates baseline and shock trajectories

**`discount_yearly_profits(yearly_scenarios, discount_rate)`** → discounted yearly
- Applies present value discounting

**`aggregate_assets_to_company(assets_discounted_yearly)`** → company yearly
- Aggregates asset yearly data to company level

**`compute_companies_financials(company_yearly, assets_yearly, discount_rate)`** → list(assets, companies)
- Computes final NPV, PD (Merton), Expected Loss metrics

**`gather_and_pivot_results(df_companies)`** → companies_pivot
- Transforms company scenario data into wide format for reporting
- Adds percentage change columns:
  - `NPV_change_pct`: Percentage change from baseline to shock NPV
  - `Expected_loss_change_pct`: Percentage change from baseline to shock expected loss
- Output columns: company, NPV_baseline, NPV_shock, NPV_change_pct, PD_baseline, PD_shock, Expected_loss_baseline, Expected_loss_shock, Expected_loss_change_pct

## Shiny Application

### Structure
- `app_ui()` / `app_server()` orchestrate modules
- Uses {golem} framework conventions

### Modules

**`mod_control`** - Control panel
- File upload, parameter inputs, run button

**`mod_hazards_events`** - Event configuration
- Three cascading dropdowns:
  1. Hazard Type (flood, heat, etc.)
  2. Scenario (CurrentClimate, RCP8.5, etc.)
  3. Return Period (10, 100, 1000 years)
- Chronic checkbox, optional shock year
- Add button, configured events table
- Output: events dataframe with event_id, hazard_type, scenario, event_year, chronic

**`mod_results_assets`** - Asset-level results display

**`mod_results_companies`** - Company-level results display
- Displays pivoted company results with formatted columns:
  - Percentage change columns: formatted as "X.XX%"
  - PD columns: multiplied by 100 and formatted as "X.XXXX%"
  - NPV and loss columns: formatted as currency "$X,XXX"

**`mod_status`** - Processing status indicator

### Running the App

**Development:**
```r
golem::run_dev()
```

**Production:**
```r
run_app(base_dir = "path/to/data")
```

## Testing

### Test Structure
- Location: `tests/testthat/`
- Test data: `tests/tests_data/`
- Naming: `test-{function_name}.R`

### Test Requirements
- EVERY new function MUST have a corresponding test file
- Use TDD: Write tests first, then implement
- Test both success and failure cases
- Use `devtools::test()` to run all tests
- Use `devtools::test_file("tests/testthat/test-function_name.R")` for specific tests

### Environment Variables for Testing
```bash
SKIP_SLOW_TESTS=TRUE devtools::test()
```

### Aggregation in Tests
- Tests use `aggregate_factor = 16` for speed
- App uses `aggregate_factor = 1` for full resolution
- Pre-aggregated test files: `global_pc_h10glob__agg16.tif`

## File Organization

### R Functions (`R/` directory)
- `app_*.R` - Main application files
- `mod_*.R` - Golem modules
- `assets__*.R` - Asset-level calculations
- `companies__*.R` - Company-level calculations
- `geospatial__*.R` - Geospatial processing
- `shock__*.R` - Shock application logic
- `utils__*.R` - Utility functions
- `compute_risk.R` - Main orchestrator

#### Key Utility Functions
- **`filter_hazards_by_events(hazards, events)`** - Filters hazard rasters by event requirements
  - For TIF hazards: exact name matching
  - For NC hazards: automatically expands to ALL ensemble variants (mean, median, p10, p90, etc.)
  - Ensures complete statistics extraction for NC files
  - See: `R/utils__filter_hazards_by_events.R`
- **`load_hazards_and_inventory(hazards_dir, aggregate_factor)`** - Loads all hazards (TIF + NC) with metadata inventory
  - Returns: `list(hazards = list(tif = ..., nc = ...), inventory = tibble(...))`
  - For NC: loads ALL ensemble variants as separate rasters
  - See: `R/utils__load_hazards.R`

### Documentation
- `man/` - Auto-generated documentation (DO NOT edit manually)
- `DESCRIPTION` - Package metadata and dependencies
- `NAMESPACE` - Auto-generated exports (DO NOT edit manually)
- Use roxygen2 tags (@export, @param, @return, @examples)
- Run `devtools::document()` to regenerate documentation

## Development Workflow

### Commands
- `golem::run_dev()` - Run app in development mode
- `devtools::test()` - Run all tests
- `devtools::test_file("path/to/test.R")` - Run specific test
- `devtools::load_all()` - Reload package changes
- `devtools::document()` - Update documentation and NAMESPACE
- `devtools::check()` - Run full package checks

### TDD Workflow
1. Write test first (red)
2. Implement minimal code to pass (green)
3. Refactor while keeping tests green
4. Run `devtools::document()` to update docs
5. Run full test suite before commit

### No Backwards Compatibility Policy
- Make changes directly, no compatibility layers
- Update ALL dependent code immediately
- Delete old implementations completely
- Use grep/codebase_search to find all usages

## Data Processing Pipeline

### Brazil Hazard Extraction

**Script:** `data-raw/process_flood_maps_brazil.R`

**Purpose:** Extract Brazil-specific subsets from global hazard data

**Workflow:**
1. Reads global rasters from `workspace/hazards_world/{hazard_type}/`
2. Loads Brazil administrative boundaries
3. Crops and masks to Brazil extent
4. Saves to `tests/tests_data/hazards/{hazard_type}/`
5. Maintains directory structure and naming

**Output naming:** `{scenario}_brazil.tif`

## Current Status

### Complete Features
- ✅ Precomputed administrative hazards (ADM1/ADM2)
- ✅ Priority cascade for hazard assignment
- ✅ Metadata-driven hazard loading
- ✅ Three-dropdown cascading UI for event selection
- ✅ Asset filtering by companies
- ✅ Complete financial calculation pipeline
- ✅ Company-level risk aggregation
- ✅ Result pivoting for reporting

### Placeholder Features (Pass-through)
- 🔄 Shock functions (acute/chronic) - maintain interface, return baseline values
- Logic to be implemented based on events dataframe

## Key Concepts

### Aggregation Factor
- Controls raster resolution for performance
- Tests: factor 16 (fast)
- Production: factor 1 (full resolution)
- Separate from return period (years)

### Return Period
- Climate event frequency (10, 100, 1000 years)
- Part of hazard metadata
- Used for event filtering

### Scenario
- Climate projection (CurrentClimate, RCP8.5, RCP4.5, etc.)
- Defined in `hazards_name_mapping.csv`

### ADM Levels
- ADM1 = Province/State level
- ADM2 = Municipality/County level

### Event Types
- **Acute**: One-time shock in specific year
- **Chronic**: Ongoing degradation over time

## Dependencies

### Required Packages (Imports)
- shiny, golem
- terra (raster processing)
- sf (spatial operations)
- dplyr, tidyr (data manipulation)
- exactextractr (fast raster extraction)

### Optional Packages (Suggests)
- testthat (testing framework)
- shinytest2 (E2E testing)

## Performance Considerations

### Optimizations
- Aggregation factor reduces raster resolution for speed
- Precomputed administrative hazards eliminate spatial joins
- Single CSV instead of multiple GeoJSON files
- Batch processing by municipality/province groups

### Trade-offs
- Higher aggregation = faster processing, lower spatial accuracy
- Precomputed data = consistency but less flexibility
- Memory usage scales with number of hazard scenarios loaded

## Error Handling

### Console Error Logging
The app includes clean, minimal console error logging to help with debugging:

- **Main App Errors**: Simple error logging in `app_server.R` with error message and location
- **Module Errors**: Module-specific error logging in control, status, and hazards events modules
- **Utility Functions**: `log_error_to_console()`, `log_module_error()`, `log_reactive_error()` for consistent error reporting

**Error Log Format**:
```
=== ERROR ===
Message: [error details]
Context: [module/function context]
Location: [exact line where error occurred]
=============
```

**Benefits**:
- Errors appear in both UI and console
- Shows exact line where error occurred
- Minimal, focused output
- Module and function identification

### Informative Errors
- Asset with no location data (no coords, municipality, or province)
- Missing hazard files referenced in mapping
- Duplicate hazard definitions on filter columns
- Assets without matching companies

### Validation
- File existence checks
- Column name validation
- Data type verification
- Referential integrity (asset → company, hazard → damage factors)

## Hazard-Specific Implementations

### Drought (SPI3) for Agriculture

**Overview**: Drought impacts are calculated using SPI3 (Standardized Precipitation Index, 3-month) droughts that affect agriculture assets only. The user selects the season when the drought occurs (Summer, Autumn, Winter, Spring).

**Damage Factor Matching**:
- **Crop Type**: Coffee, Corn, Soybean, Sugarcane, or "Other" (default for missing subtypes)
- **Province**: Uses asset province or falls back to "Other" if not found
- **Season Matching**:
  - **On-season**: User-selected season matches crop growing season → full damage_factor applied
  - **Off-season**: Seasons don't match → damage_factor multiplied by off_window coefficient
- **Intensity Capping**:
  - SPI3 < -3: capped to -3 (maximum damage)
  - SPI3 > -1: damage_factor = 0 (no damage)
  - -3 ≤ SPI3 ≤ -1: use actual intensity

**Revenue Shock Formula**:
- On-season: `Revenue × (1 - damage_factor)`
- Off-season: `Revenue × (1 - damage_factor × off_window)`

**Implementation Files**:
- UI: `R/mod_hazards_events.R` - Season dropdown for Drought events
- Matching: `R/geospatial__join_damage_cost_factors.R` - `join_drought_damage_factors()`
- Shock Application: `R/shock__apply_acute_revenue_shock.R` - `apply_drought_shock()`

**Data Requirements**:
- `damage_and_cost_factors.csv` must include rows with:
  - `hazard_type = "drought"`, `hazard_indicator = "SPI3"`, `metric = "mean"`
  - Columns: `province`, `subtype`, `season`, `damage_factor`, `off_window`
- Events must include `season` column (Summer/Autumn/Winter/Spring)

## Recent Changes

### Bug Fixes
- **Fixed NC hazard scenario extraction**: Corrected parsing logic in `load_nc_cube_with_terra()` to properly handle both GIRI-style files (explicit scenario indices like `scenario=_1`) and ensemble-style files (combination indices). Files now correctly extract all scenarios instead of defaulting to "present" only.
- **Fixed hazard selection validation**: Added proper validation to require at least one hazard event selection before running analysis. Previously, the app would run with a default hazard when none were selected, which could lead to unexpected results. Now shows clear error message: "Please select at least one hazard event before running the analysis. Use the 'Add hazard' button to configure hazard events."
- Fixed encoding issues in Brazilian flood map processing
