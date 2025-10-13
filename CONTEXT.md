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
│   ├── asset_information.csv
│   └── company.csv
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

#### 1. `asset_information.csv`
Columns: asset_id, company_id, asset_category, size_in_m2, location info (lat/lon OR municipality OR province)

#### 2. `company.csv`
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

The tool supports two hazard data formats:

#### GeoTIFF Files (.tif)
Location: `{base_dir}/hazards/{hazard_type}/`

Naming convention: `global_{scenario_code}_h{return_period}glob.tif`

Examples:
- `global_pc_h10glob.tif` - Current climate, 10-year return period
- `global_rcp85_h100glob.tif` - RCP8.5, 100-year return period

**Metadata:** Defined in `hazards_metadata.csv` (hazard_file, hazard_type, scenario_code, scenario_name, hazard_return_period)

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
- `ensemble`: ALL variants loaded separately (mean, median, p10, p90, etc.)

**Georeferencing:** NC files store lat/lon as cell centers. Loader calculates resolution and extends extent by half-pixel to create proper raster edges.

**NC Ensemble Handling:** Each ensemble variant (mean, median, p10, p90) is loaded as a separate `SpatRaster` with naming convention: `{hazard_type}__{hazard_indicator}__GWL={level}__RP={period}__ensemble={variant}`. This enables direct extraction of pre-computed statistics without spatial computation.

## Key Functions

### Main Orchestrator

**`compute_risk(assets, companies, events, hazards, precomputed_hazards, damage_factors, growth_rate, net_profit_margin, discount_rate)`**
- Returns: `list(assets, companies, assets_yearly, companies_yearly)`
- Orchestrates entire pipeline from raw inputs to final risk metrics
- Filters assets to only those with matching companies
- Uses priority cascade for hazard assignment

### Data Loading

**`read_assets(base_dir)`** → data.frame
- Reads from `{base_dir}/user_input/asset_information.csv`

**`read_companies(file_path)`** → data.frame
- Reads company data from specified path

**`read_damage_cost_factors(base_dir)`** → data.frame
- Reads from `{base_dir}/damage_and_cost_factors.csv`

**`read_precomputed_hazards(base_dir)`** → data.frame
- Reads from `{base_dir}/precomputed_adm_hazards.csv`


### Hazard Loading Workflow

**1. `load_hazards_and_inventory(hazards_dir, aggregate_factor = 1L)`** → list(hazards, inventory)
- **Unified loader** for both TIF and NetCDF files
- Scans for TIF mapping file (`hazards_metadata.csv`); if absent, skips TIF loading
- Auto-discovers NC files by scanning directory tree
- Returns: `list(hazards = list(tif = ..., nc = ...), inventory = tibble(...))`
- **TIF**: Loads from `hazards_metadata.csv` using `load_hazards_from_mapping()`
- **NC**: Auto-discovers files, parses dimensions, creates one SpatRaster per (GWL × return_period × ensemble) combination
  - Each ensemble variant (mean, median, p10, p90) becomes a separate raster
  - Naming: `{type}__{indicator}__GWL={level}__RP={period}__ensemble={variant}`
- **Inventory**: Combined metadata tibble with `source` column ("tif" or "nc")

**Application Usage:**
```r
# In mod_control_server:
hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
# Access hazards (flattened for compute pipeline):
hazards_flat <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
# Access inventory (for UI dropdowns):
inventory <- hazard_data$inventory
```

**Naming Convention:**
- TIF: `{hazard_type}__{scenario_code}_h{return_period}glob` (e.g., `flood__pc_h10glob`)
- NC: `{hazard_type}__{indicator}__GWL={gwl}__RP={rp}__ensemble=mean` (e.g., `Drought__CDD__GWL=present__RP=10__ensemble=mean`)

### Geospatial Processing

**`create_asset_geometries(assets_df, default_buffer_size_m, output_crs)`** → sf object
- Creates point geometries with buffers for assets WITH coordinates only
- Uses `size_in_m2` for buffer sizing
- Raises error if coordinates missing

**`extract_hazard_statistics(assets_df, hazards, hazards_inventory, precomputed_hazards, use_exactextractr)`** → long format data.frame
- **Dual workflow dispatcher** based on `source` column in `hazards_inventory`:
  - **TIF sources** → `extract_tif_statistics()`: Spatial computation (crop, mask, compute stats from pixel values)
  - **NC sources** → `extract_nc_statistics()`: Direct extraction of pre-computed ensemble statistics
- **Priority cascade** for asset location:
  1. Coordinates → spatial extraction from rasters
  2. No coordinates + municipality → precomputed ADM2 lookup
  3. No coordinates + province → precomputed ADM1 lookup
  4. None → Error
- Returns long format with columns: `hazard_mean`, `hazard_median`, `hazard_p10`, `hazard_p90`, etc.
- For NC: populates ensemble columns by extracting point values from each ensemble raster
- For TIF: computes statistics from masked pixel values within asset buffer

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

