# Climate Risk Tool - AI Agent Context

## Project Overview

R package built with {golem} framework for Shiny apps. Performs climate risk analysis on assets and companies using geospatial hazard data.

## Core Architecture

### Data Pipeline

The tool processes climate risk through a multi-step pipeline orchestrated by `compute_risk()`:

**PHASE 0: Input Preparation**
1. **Province Assignment**: Assign provinces to assets without province data (via coordinates or municipality)
2. **Input Validation**: Validate data coherence (if `validate_inputs=TRUE` and `base_dir` provided)
3. **Asset Filtering**: Filter assets to only include those with matching companies

**PHASE 1: Geospatial Processing**
4. **Geospatial Extraction**: Assign hazard values to assets using priority cascade (coordinates → municipality → province)
5. **Hazard-Damage Mapping**: Join damage cost factors based on hazard intensity and asset characteristics

**PHASE 2: Financial Modeling**
6. **Baseline Trajectories**: Compute revenue and profit projections without climate shocks
7. **Shock Application**: Apply acute climate event shocks to revenue and profits
8. **Scenario Building**: Combine baseline and shock trajectories
9. **Discounting**: Apply present value discounting to future cash flows

**PHASE 3: Risk Aggregation**
10. **Company Aggregation**: Roll up asset-level to company-level metrics
11. **Risk Metrics**: Compute NPV, PD (Merton), Expected Loss
12. **Result Formatting**: Pivot to wide format for reporting

### Hazard Assignment Priority Cascade

Assets are assigned hazard values using this priority:
1. **Coordinates** (lat/lon) → Spatial extraction from raster files
2. **Municipality** (ADM2) → Precomputed lookup from `precomputed_adm_hazards.csv`
3. **Province** (ADM1) → Precomputed lookup from `precomputed_adm_hazards.csv`
4. **None** → Error with informative message

This is handled automatically by `extract_hazard_statistics()`.

### Hazard Configuration System

The tool uses a **unified hazard configuration architecture** that supports both single-indicator and multi-indicator hazards transparently.

#### Hazard Types

**Single-Indicator Hazards** (1 data source per hazard):
- **Flood**: Flood depth (cm)
- **Compound**: Compound climate index
- **Drought**: Drought index (seasonal)

**Multi-Indicator Hazards** (multiple data sources combined):
- **Fire**: Combines 3 indicators:
  - `land_cover`: Static land cover classification (TIF)
  - `FWI`: Fire Weather Index max value (NetCDF)
  - `days_danger_total`: Days with significant fire weather (NetCDF)

#### Configuration Registry

Defined in `R/config__hazard_types.R`:

```r
get_hazard_type_config() → list(
  Fire = list(
    indicators = c("land_cover", "FWI", "days_danger_total"),
    primary_indicator = "FWI",  # Drives UI dropdowns
    description = "..."
  ),
  Flood = list(...),
  ...
)
```

Helper functions:
- `is_multi_indicator_hazard(hazard_type)` → TRUE/FALSE
- `get_primary_indicator(hazard_type)` → indicator name
- `get_required_indicators(hazard_type)` → vector of indicators

#### UI Inventory Filtering

The UI only shows:
- **Hazard Type** (e.g., "Fire", "Flood")
- **Scenario** (e.g., "SSP2-4.5", "CurrentClimate")
- **Return Period** (e.g., 10, 50, 100 years)

The `hazard_indicator` dimension is **completely hidden** from users.

Implementation:
1. `load_hazards_and_inventory()` loads full inventory (all indicators)
2. `filter_inventory_for_ui()` filters to only primary indicators
3. UI dropdowns populated from filtered inventory
4. Multi-indicator complexity handled internally

#### Event Expansion

When users select a multi-indicator hazard (e.g., Fire), the system automatically expands it:

**User Selection:**
```
Event: Fire, SSP2-4.5, RP=10, year=2030
```

**Internal Expansion (by `expand_multi_indicator_events()`):**
```
Event 1: Fire/land_cover,  present,   RP=0,  year=2030
Event 2: Fire/FWI,         SSP2-4.5,  RP=10, year=2030
Event 3: Fire/days_danger_total, SSP2-4.5, RP=10, year=2030
```

All three events:
- Share same `event_id` and `event_year`
- Each has correct `hazard_name` from inventory
- Static indicators (land_cover) use their own scenario/RP
- Dynamic indicators (FWI, days_danger_total) use user-selected scenario/RP

This expansion happens in `compute_risk()` before hazard extraction.

#### Adding New Hazard Types

To add a new hazard type:

1. **Add data files** to `{base_dir}/hazards/{hazard_type}/`
2. **Update metadata:**
   - For TIF: Add to `hazards_metadata.csv`
   - For NC: Auto-discovered by `load_nc_hazards_with_metadata()`
3. **Update configuration** in `R/config__hazard_types.R`:
   ```r
   NewHazard = list(
     indicators = c("indicator1", "indicator2"),
     primary_indicator = "indicator1",
     description = "..."
   )
   ```
4. **Add damage calculation** in `R/geospatial__join_damage_cost_factors.R` if needed
5. **Add shock application** in `R/shock__apply_acute_*.R` if needed

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

- Incremental refresh logic: `data-raw/precompute_hazards.py` drops any existing rows sharing the same (`region`, `adm_level`, `hazard_type`, `hazard_indicator`, `scenario_name`, `hazard_return_period`, `ensemble`, `season`) keys before appending newly computed results. This guarantees a clean overwrite when hazards are reprocessed.
- Metadata alignment: `load_hazards_metadata(metadata_path)` (Python helper in `data-raw/precompute_hazards.py`) loads `hazards_metadata.csv` and enforces that GeoTIFF-derived scenario names and indicators use the curated metadata instead of filename heuristics.

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

**`compute_risk(assets, companies, events, hazards, precomputed_hazards, damage_factors, growth_rate, discount_rate)`**
- Returns: `list(assets, companies, assets_yearly, companies_yearly)`
- Orchestrates entire pipeline from raw inputs to final risk metrics
- Filters assets to only those with matching companies
- Uses priority cascade for hazard assignment
- Uses company-specific net profit margins from the companies data frame

### Input Data Validation

**`validate_input_coherence(assets_df, damage_factors_df, precomputed_hazards_df, cnae_exposure_df, adm1_names, adm2_names)`**
- Performs comprehensive validation checks on all input data for coherence and consistency
- Called automatically by `compute_risk()` if `base_dir` and `validate_inputs=TRUE` are provided
- Can be called manually before running analysis to catch data issues early
- Stops execution if validation errors are found; returns list with `errors` and `warnings` vectors

**Validation Checks:**

1. **Province Names in Damage Factors**: All province names must match ADM1 boundary names (after ASCII normalization)
2. **Province Names in Assets**: All asset province names must match ADM1 boundary names
3. **Municipality Names in Assets**: All asset municipality names must match ADM2 boundary names  
4. **Province Names in Precomputed Hazards**: All province-level (ADM1) regions must match ADM1 boundary names
5. **Municipality Names in Precomputed Hazards**: All municipality-level (ADM2) regions must match ADM2 boundary names
6. **CNAE Codes in Assets**: All CNAE codes in assets must exist in the reference CNAE exposure file
7. **Share of Economic Activity**: For each company, asset shares must sum to 1.0 (±0.01 tolerance)

**ASCII Normalization**: All province and municipality names are normalized using `stringi::stri_trans_general("Latin-ASCII")` to remove accents (e.g., "Espírito Santo" → "Espirito Santo"). This ensures consistent matching between data sources.

**Helper Functions**:
- `load_adm1_province_names(base_dir)` → Character vector of normalized ADM1 province names
- `load_adm2_municipality_names(base_dir)` → Character vector of normalized ADM2 municipality names

**Implementation**: `R/utils__validate_inputs.R`
**Tests**: `tests/testthat/test-utils__validate_inputs.R`

### Data Loading

**`read_assets(base_dir)`** → data.frame
- Reads from `{base_dir}/user_input/asset_information.xlsx`
- ASCII-normalizes province and municipality names
- **Does NOT assign provinces to assets** - this is now done in `compute_risk()` or can be called separately

**`assign_province_to_assets(assets_df, base_dir)`** → data.frame
- Assigns provinces to assets without province data using spatial matching
- Strategy 1: Uses coordinates (lat/lon) for spatial join with ADM1 boundaries
- Strategy 2: Uses municipality name to look up province
- Called automatically by `compute_risk()` if `base_dir` is provided
- Can be called manually: `assets <- assign_province_to_assets(assets, base_dir)`

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
- Raises explicit errors listing any missing hazards when precomputed data is incomplete for an asset
- Returns `matching_method = "municipality"` or `"province"`
- Raises detailed errors if region or hazard combo not found

**`join_damage_cost_factors(assets_long_format, damage_factors_df, cnae_exposure = NULL)`** → data.frame
- Joins damage and cost factors based on hazard type:
  - **Flood**: Joins on hazard_type, hazard_indicator, rounded hazard_intensity, and asset_category
  - **Compound**: Joins on hazard_type, province, scenario_name (GWL), and metric (sector-based from CNAE exposure)
  - **Drought**: Joins on province, crop subtype, season, and closest hazard_intensity match
- Optional `cnae_exposure` parameter used for Compound hazards to determine metric (high/median/low) based on sector CNAE codes

### Financial Calculations

**`filter_assets_by_companies(assets, companies)`** → filtered assets
- Filters assets to only include those with companies in companies data

**`compute_baseline_trajectories(baseline_assets, companies, growth_rate)`** → yearly baseline
- Computes baseline revenue and profit trajectories over time
- Uses company-specific net profit margins from the companies data frame

**`compute_shock_trajectories(yearly_baseline, assets_with_factors, events, companies)`** → shocked yearly
- Applies acute shocks to revenue and profits
- Uses company-specific net profit margins from the companies data frame

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
- Shock year input
- Add button, configured events table
- Output: events dataframe with event_id, hazard_type, scenario, event_year

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

### Recent Updates
- Added `test-mod_profit_pathways.R` to cover log-scale clipping logic for non-positive asset profits so charts remain informative.
- Added drought zero-flooring regression test in `test-shock__apply_acute_revenue_shock.R` to lock revenue at or above zero for extreme damage factors across hazards.
- Added regression coverage in `test-geospatial__extract_hazard_statistics.R` ensuring `extract_precomputed_statistics()` fails fast with explicit hazard names when precomputed data is missing.

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
- 🔄 Shock functions (acute) - maintain interface, return baseline values
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
- **Multi-Season Crops** (NEW): Some crops have multiple growing seasons (e.g., Sugarcane in Alagoas has Winter 37% and Autumn 35%)
  - System finds ALL growing seasons for the crop at the matched intensity level
- **Season Matching Logic**:
  - **Exact Match**: User-selected season matches one of the crop's growing seasons
    - Use that specific season's damage_factor directly
    - `growing_season` column shows the matched season (e.g., "Winter")
  - **No Match**: User-selected season doesn't match any growing season (off-season)
    - Calculate: `avg_damage_factor = mean(all growing season damage factors)`
    - Calculate: `avg_off_window = mean(all growing season off_windows)`
    - Apply: `final_damage_factor = avg_damage_factor × avg_off_window`
    - `growing_season` column shows "Averaged (Season1, Season2, ...)" with seasons sorted alphabetically
- **Intensity Capping**:
  - SPI3 < -3: capped to -3 (maximum damage)
  - SPI3 > -1: damage_factor = 0 (no damage)
  - -3 ≤ SPI3 ≤ -1: use actual intensity

**Output Columns** (NEW):
Assets output now includes drought metadata:
- `season`: User-selected drought season
- `growing_season`: Matched growing season name or "Averaged (...)" for off-season
- `off_window`: Off-window coefficient value used
- `damage_factor`: Final applied damage factor

**Revenue Shock Formula**:
- Exact season match: `Revenue × (1 - damage_factor)`
- Off-season/averaged: `Revenue × (1 - avg_damage_factor × avg_off_window)`

**Example - Sugarcane in Alagoas**:
- Growing seasons: Winter (37%, off=30%), Autumn (35%, off=30%)
- User selects Winter → damage_factor = 0.37, growing_season = "Winter"
- User selects Autumn → damage_factor = 0.35, growing_season = "Autumn"
- User selects Summer → avg_damage = 0.36, avg_off = 0.30, final damage = 0.108 (36% × 30%), growing_season = "Averaged (Autumn, Winter)"

**Implementation Files**:
- UI: `R/mod_hazards_events.R` - Season dropdown for Drought events
- Matching: `R/geospatial__join_damage_cost_factors.R` - `join_drought_damage_factors()`
- Shock Application: `R/shock__apply_acute_revenue_shock.R` - `apply_drought_shock()`

### Compound (Heat) for All Assets

**Overview**: Compound (heat) impacts affect all asset categories through labor productivity loss calculated using Cobb-Douglas production function. Damage factors vary by province, Global Warming Level (GWL), and sector-based labor productivity exposure (high/median/low).

**Damage Factor Matching**:
- **Province**: Uses asset province for geographic matching
- **GWL**: Uses scenario_name from events (matches gwl column in damage_factors)
- **Metric Selection** (new): Based on sector CNAE code:
  - If asset has sector (CNAE code) and found in CNAE exposure file → use corresponding LP exposure value (high/median/low)
  - If sector is missing/NA → use "median" (default)
  - Exception: If sector is missing AND `asset_category == "agriculture"` → use "high"
- **Join**: Matches on `hazard_type`, `province`, `gwl`, AND `metric` (not hardcoded to "median")

**Revenue Shock Formula**:
- Uses Cobb-Douglas production function to calculate labor productivity loss
- Formula: `weighted_lp_loss = (hazard_intensity / 365) × damage_factor`
- Then adjusts labor input and calculates output change

**Implementation Files**:
- Data Loading: `R/utils__read_inputs.R` - `read_cnae_labor_productivity_exposure()`
- Matching: `R/geospatial__join_damage_cost_factors.R` - `join_compound_damage_factors()` (now accepts `cnae_exposure` parameter)
- Shock Application: `R/shock__apply_acute_revenue_shock.R` - `apply_compound_shock()` (unchanged)

**Data Requirements**:
- `damage_and_cost_factors.csv` must include rows with:
  - `hazard_type = "Compound"`, `metric` in ("high", "median", "low")
  - Columns: `province`, `gwl`, `metric`, `damage_factor`
- `cnae_labor_productivity_exposure.xlsx` with columns: `cnae` (numeric), `sector`/`description`, `decision`/`lp_exposure` ("High"/"Median"/"Low" normalized to lowercase)
- Assets should have `sector` column with numeric CNAE codes (no leading zeros, e.g., 6 not 06)

**Data Requirements**:
- `damage_and_cost_factors.csv` must include rows with:
  - `hazard_type = "drought"`, `hazard_indicator = "SPI3"`, `metric = "mean"`
  - Columns: `province`, `subtype`, `season`, `damage_factor`, `off_window`
- Events must include `season` column (Summer/Autumn/Winter/Spring)

### Fire for Buildings and Agriculture

**Overview**: Fire impacts use a compound indicator approach combining land cover risk, maximum Fire Weather Index (FWI), and days with significant fire weather. Fire affects both buildings (profit shock) and agriculture (revenue shock).

**Hazard Indicators** (all three used simultaneously - unique multi-indicator approach):
- `land_cover`: Categorical raster (2024_brazil_land_cover.tif), extracted using mode (most common value)
- `FWI`: Fire Weather Index maximum value from `FWI/ensemble_return_period.nc`, capped at 50
- `days_danger_total`: Number of days per year with significant fire risk from `days_danger_total/ensemble_return_period.nc`

**Note**: Fire is unique in requiring three hazard indicators to compute a single damage value. Unlike other hazards (Flood, Drought, Compound) which use one indicator per hazard type, Fire combines all three indicators in its damage calculation.

**Damage Formula**:
- **Commercial/Industrial buildings** (profit shock):
  ```
  Fire Damage = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365) × cost_factor
  Profit_shocked = Profit - Fire Damage
  ```
- **Agriculture assets** (revenue shock):
  ```
  Fire Damage = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365)
  Revenue_shocked = Revenue × (1 - Fire Damage)
  ```

**Land Cover Risk Determination**:
- Assets **with coordinates**: Extract mode (most common) land cover code from raster within asset buffer
- Join extracted code with `land_cover_legend_and_index.xlsx` to get risk metric (0.25, 0.50, 0.75, or 1.00)
- Assets **without coordinates**: Default to 0.50 (50% risk)
- Land cover categories: Forest (0.50), Grassland (1.00), Urban Area (0.25), Agriculture (0.50-0.75), etc.

**FWI Capping**: FWI values are capped at maximum 50 before damage factor lookup. Higher values use the damage factor for FWI=50.

**Shock Application Order**:
- **Revenue shock phase** (agriculture): Applied in event_id order alongside Drought and Compound shocks
- **Profit shock phase** (buildings): Applied in event_id order alongside Flood shocks
- Profits can become negative from Fire damage (as with other profit shocks)

**Implementation Files**:
- Data loading: `R/utils__read_inputs.R` - `read_land_cover_legend()`
- Extraction: `R/geospatial__extract_hazard_statistics.R` - mode aggregation for categorical land cover
- Damage factors: `R/geospatial__join_damage_cost_factors.R` - `join_fire_damage_factors()`
- Revenue shock: `R/shock__apply_acute_revenue_shock.R` - `apply_fire_revenue_shock()`
- Profit shock: `R/shock__apply_acute_profit_shock.R` - Fire case in event loop

**Data Requirements**:
- `damage_and_cost_factors.csv` must include rows with:
  - `hazard_type = "Fire"`, `hazard_indicator = "FWI"`, `hazard_intensity` = 0 to 50
  - Columns: `asset_category` (commercial building/industrial building/agriculture), `damage_factor`, `cost_factor`
- `land_cover_legend_and_index.xlsx` with columns: `Code`, `Class`, `Category`, `Risk`
- Hazard files:
  - `hazards/Fire/land_cover/2024_brazil_land_cover.tif`
  - `hazards/Fire/FWI/ensemble_return_period.nc`
  - `hazards/Fire/days_danger_total/ensemble_return_period.nc`

**Multi-Indicator Architecture**:
Fire is unique in requiring three hazard indicators simultaneously. The existing hazard loading system supports this naturally through the folder structure. During damage factor calculation, the three indicators are pivoted from long format (3 rows per asset) to wide format (1 row with 3 columns) for the combined damage calculation.

**Unified Hazard Behavior**:
The system supports both single-indicator and multi-indicator hazards through a unified interface where **hazard_indicator is completely hidden from the user**:

**User Interface (UI)**:
- User selects: Hazard Type → Scenario → Return Period
- NO hazard_indicator dropdown visible
- For all hazards (Flood, Drought, Compound, Fire), the selection process is identical

**Internal System Behavior**:

1. **Single-Indicator Hazards** (Flood, Drought, Compound):
   - User selects: Flood + CurrentClimate + 100 years
   - System internally finds: 1 indicator (depth(cm))
   - Extracts: That 1 indicator
   - Damage calculation: Uses that indicator directly

2. **Multi-Indicator Hazards** (Fire):
   - User selects: Fire + GWL=2.0 + 50 years
   - System internally finds: Representative indicator (e.g., FWI)
   - `expand_fire_events()` creates 3 events: land_cover, FWI, days_danger_total
   - Extracts: ALL 3 indicators
   - Damage calculation: Combines all 3 indicators into single damage value

**Implementation Details**:
- **UI Module** (`mod_hazards_events`): Completely removed hazard_indicator dropdown; cascading is now hazard_type → scenario → return_period
- **Event Creation**: Internally stores hazard_indicator but user never sees it
- **Event Expansion**: `expand_fire_events()` detects Fire and creates 3 internal events from 1 user selection
- **Extraction**: Each indicator extracted separately, identified by hazard_type + hazard_indicator + scenario + return_period
- **Damage Joining**: Fire-specific logic (`join_fire_damage_factors()`) pivots 3 indicators to wide format and combines them
- **Shock Application**: Final damage_factor column works identically for all hazards

**Key Benefits**:
- **UI Simplicity**: User interface is identical for all hazards - no special Fire handling visible
- **Internal Flexibility**: System handles 1-to-N indicator mapping transparently
- **Extensibility**: Future multi-indicator hazards follow the same pattern
- **Backward Compatibility**: Existing single-indicator hazards work exactly as before, just without visible indicator selection

## Recent Changes

### UI & Configuration Enhancements (2025-11-12)
- Relocated hazard configuration upload from `mod_hazards_events` to the Data Upload section in `mod_control` (below the company file input), keeping the download button in the Hazard Events section; wired the upload through a `load_config()` function exposed by `mod_hazards_events_server` so analysts can load pre-configured event lists early in the workflow; coverage updated in `tests/testthat/test-mod_control.R` and `tests/testthat/test-mod_hazards_events.R`.
- Added Excel-based hazard configuration download button (styled with `btn-info` for clear differentiation from the `Add hazard` action) to the Hazard Events section, enabling analysts to save and share hazard event selections.
- Alphabetized hazard panels in the asset exposure view to improve scanability and updated the corresponding expectations in `tests/testthat/test-mod_results_assets.R`.
- Retitled the growth rate slider to `Revenue Growth (%)` so the financial parameter reflects the business terminology used in stakeholder reviews.
- Alphabetized hazard type dropdown choices in `mod_hazards_events` to match the sorted asset exposure panels.

### UI & Visualization Enhancements (2025-11-06)
- Rebranded the interface as the **Physical Risk Analysis Tool**, refreshed the subtitle, reordered the primary analysis tabs (Asset Analysis → Profit Pathways → Company Analysis → Company Results → Parameters & Status), and simplified the growth rate control label in the sidebar.
- Refined `mod_results_assets` to present hazard-specific asset tables via collapsible panels, restore original province/municipality names, surface company/sector metadata (using CNAE descriptions for sector names and retaining sector codes), expose `event_id` with formatted economic share values, and add CSV/XLSX downloads for the full asset dataset; supporting coverage added in `tests/testthat/test-mod_results_assets.R`.
- Enriched profit pathway analytics by merging company, sector, and economic-share metadata into trajectory data, preferring sector names in the selection table, and exposing CSV/XLSX downloads (`download_profit_pathways_csv`, `download_profit_pathways_excel`); validated in the new `tests/testthat/test-mod_profit_pathways.R`.
 - Enriched profit pathway analytics by merging company, sector, and economic-share metadata into trajectory data, preferring sector names in the selection table, exposing CSV/XLSX downloads (`download_profit_pathways_csv`, `download_profit_pathways_excel`), and documenting the log-scale handling of zero/negative profits directly in the UI; validated in the new `tests/testthat/test-mod_profit_pathways.R`.
- Ensured Profit Pathways renders sector names using CNAE labor descriptions while the asset results continue to surface numeric sector codes alongside the resolved descriptions, leveraging the preloaded CNAE exposure lookup; tightened coverage in `tests/testthat/test-mod_results_assets.R` and `tests/testthat/test-mod_profit_pathways.R`.
- Relocated company financial results into the `mod_company_analysis` module (removing the standalone Company Results tab), added CSV/XLSX downloads, and refreshed table/chart styling to use the Brazil palette.
- Updated the status view to show Event IDs directly in the configured hazard list, validated by the strengthened checks in `tests/testthat/test-mod_status.R`.
- Applied a Brazil-themed palette (green, yellow, blue, white) across CSS, plotly visuals, and helper utilities to align the UI and charts with the national identity.

### Bug Fixes
- **Fixed Windows path parsing in hazard loading**: Replaced fragile absolute path parsing with robust cross-platform relative path parsing in `load_nc_hazards_with_metadata()` and `load_csv_hazards_with_metadata()`. Previously, path parsing relied on finding the "hazards" directory in absolute paths, which failed on Windows due to differences in `normalizePath()` behavior and path separators. Now uses `normalizePath(..., winslash = "/")` to ensure consistent forward slashes across platforms, then computes relative paths from the known `hazards_dir` parameter. This ensures hazard_type and hazard_indicator are parsed correctly on all platforms. (2025-10-30)
- **Fixed drought damage factor matching with province fallback**: Enhanced `join_drought_damage_factors()` to handle provinces without specific drought damage data. When a province doesn't have drought factors for a crop (e.g., Amapá province), the function now falls back to the first available province that has data for that crop type. This ensures all agriculture assets affected by drought get proper damage factors, growing_season, and off_window columns. Previously, assets in provinces without drought data would get damage_factor=0 with NA metadata. (2025-10-30)
- **Fixed NC hazard scenario extraction**: Corrected parsing logic in `load_nc_cube_with_terra()` to properly handle both GIRI-style files (explicit scenario indices like `scenario=_1`) and ensemble-style files (combination indices). Files now correctly extract all scenarios instead of defaulting to "present" only.
- **Fixed hazard selection validation**: Added proper validation to require at least one hazard event selection before running analysis. Previously, the app would run with a default hazard when none were selected, which could lead to unexpected results. Now shows clear error message: "Please select at least one hazard event before running the analysis. Use the 'Add hazard' button to configure hazard events."
- Fixed encoding issues in Brazilian flood map processing
