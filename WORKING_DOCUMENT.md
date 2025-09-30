# Climate Risk Tool - Working Document

Purpose: living reference for code structure, data schemas, and test plan. Keep this updated when adding functions or tests.

## 1) Shiny Application (UI/Server)

- `app_ui()`/`app_server()` orchestrate modules.
- Module: `mod_hazards_events_ui/server`
  - UI elements: Hazard select, Scenario select (dependent), Chronic checkbox, optional Shock year, Add hazard button, Configured events table.
  - Output: stored events with fields `event_id`, `hazard_type`, `scenario`, `event_year` (NA if chronic), `chronic`.
- Startup: hazards inventory loads immediately from `golem::get_golem_options("base_dir")` and populates the dropdowns.

## 2) Core Split Pipeline (Final)

- ✅ `compute_risk(assets, companies, events, hazards, areas, damage_factors, growth_rate, net_profit_margin, discount_rate)` → list(assets, companies, assets_yearly, companies_yearly)
  - **UPDATED**: Now uses direct geospatial pipeline for speed optimization through load_hazards aggregation factor instead of precomputation.
  - Orchestrates: geospatial processing → yearly trajectory computations → company aggregation.
- ✅ `compute_hazard_events(assets, hazards, areas, damage_factors)` → assets in long format with geospatial hazard data and damage/cost factors joined by hazard_type.
- ✅ Pipeline now uses long format hazard data throughout, with hazard_type column enabling proper joins with damage cost factors.
- 🔄 Shock functions are currently placeholders - they pass through baseline values while maintaining the expected interface.

Event combination rule: worst-case per asset (min share). Configurable later.

## 3) Supporting Core Functions

- user_input: `asset_information.csv`, `company.csv`
- areas: province GeoJSON (ADM1), municipality GeoJSON (ADM2)
- hazards: .tif rasters
- damage_and_cost_factors.csv

## Function contracts (implemented via TDD)

### Workflow in app
1. User configures events via module (can add many). Stored list drives analysis.
2. Prepare assets with geospatial hazard data using `compute_hazard_events`.
3. Process events, combine to shocked asset shares, build scenarios, and compute asset-level financials with `compute_assets_financials`.
4. Aggregate to company-level risk metrics using `compute_companies_financials`.

### Individual Pipeline Functions  
- ✅ read_assets(base_dir) -> data.frame - reads asset CSV file from base_dir/user_input/, converts to snake_case, parses numeric columns
- ✅ read_companies(file_path) -> data.frame - reads company CSV file from specified path, converts to snake_case, parses numeric columns
- ✅ read_damage_cost_factors(base_dir) -> data.frame - reads damage and cost factors CSV from base_dir/, handles comma decimal separators, converts to snake_case
- ✅ load_hazards(hazards_dir) -> named list of SpatRaster objects from .tif files; searches recursively in hazard-type subfolders (e.g., `floods/`, `heat/`) and names layers as `hazardType__scenario` based on subfolder and filename (sans extension)
- ✅ load_location_areas(municipalities_dir, provinces_dir) -> list(municipalities, provinces) - loads both area types at once
- ✅ load_municipalities(municipalities_dir) -> named list of sf objects from .geojson files
- ✅ load_provinces(provinces_dir) -> named list of sf objects from .geojson files  
- ✅ geolocate_assets(assets, hazards, municipalities_areas, provinces_areas) -> assets_with_geometry - adds geometry and centroid columns using lat/lon > municipality > province priority, uses size_in_m2 for lat/lon buffer sizing, now includes geolocation_method column
- ✅ cutout_hazards(assets_with_geometry, hazards) -> assets_with_hazard_values - extracts hazard raster values for each asset geometry, optimized to group by municipality/province for efficiency
- ✅ summarize_hazards(assets_with_hazard_values) -> assets_long_format - **NEW**: transforms to long format with hazard_name, hazard_type, hazard_intensity columns (one row per asset-hazard combination)
- ✅ join_damage_cost_factors(assets_long_format, damage_factors_df) -> assets_with_factors - **UPDATED**: joins on hazard_type, rounded hazard_intensity, and asset_category using dataframe parameter
- ✅ compute_hazard_events(assets, hazards, areas, damage_factors) -> assets_with_factors - **UPDATED**: orchestrates geolocation, cutout, summarize, and join operations in one function
- 🔄 apply_acute_shock_yearly(yearly_trajectories, assets_factors, acute_events) -> shocked_trajectories - **REFACTORED**: now takes events dataframe as input, currently passes through values unchanged (shock logic to be implemented)
- 🔄 apply_chronic_shock_yearly(yearly_trajectories, assets_factors, chronic_events) -> shocked_trajectories - **REFACTORED**: now takes events dataframe as input, currently passes through values unchanged (shock logic to be implemented)
- ✅ compute_shock_trajectories(yearly_baseline, assets_with_factors, events) -> shocked_yearly - **REFACTORED**: splits events into acute/chronic dataframes, applies shocks sequentially (acute first, then chronic), removes metadata aggregation logic
- ✅ build_yearly_scenarios(baseline_yearly, shocked_yearly) -> combined_scenarios - concatenates baseline and shock yearly trajectories
- ✅ compute_baseline_trajectories(baseline_assets, companies, growth_rate, net_profit_margin) -> yearly_baseline - computes baseline revenue and profit trajectories over time
- ✅ discount_yearly_profits(yearly_scenarios, discount_rate) -> discounted_yearly - applies present value discounting to yearly trajectories
- ✅ compute_company_yearly_trajectories(assets_discounted_yearly) -> company_yearly - aggregates asset yearly data to company level
- ✅ compute_companies_financials(company_yearly, assets_yearly, discount_rate) -> list(assets, companies) - computes final NPV, PD, and Expected Loss metrics
- ✅ gather_and_pivot_results(df_assets, df_companies) -> list(assets_pivot, companies_pivot) - transforms scenario data into wide format for reporting

## 4) Testing strategy
- Snapshot representative rows after each stage
- Validate column presence, types, and row counts; minimal numeric checks
- Use deterministic seeds and small polygons

### Shiny Interface TDD
- UI contract tests:
  - `test-app_ui.R` asserts presence of `company_file` fileInput, `run_analysis` button, and `download_results` control. **NOTE**: `base_dir` is now provided via `run_app(base_dir = "path")`, not as UI input.
- Server contract tests:
  - `test-app_server.R` uses `testServer(app_server)` with `golem::with_golem_options()` and assumes:
    - Reads `base_dir` from golem options only (set via `run_app(base_dir = "path")`)
    - Requires company CSV file upload via `input$company_file`
    - Uses `read_assets()` for asset data from base_dir and `read_companies()` for uploaded company file
    - Exposes `data_loaded`, `results_ready`, and `results` in server scope
    - `results` is a list with `assets` and `companies` after running
- End-to-end test:
  - `test-app_e2e.R` with `shinytest2` drives upload→run→download flow; expects presence of `results_table`/`results_summary`/`status_text` output; handles both input-based and golem-option-based base_dir patterns

These tests are written ahead of implementation to guide UI/module construction. Update selectors/IDs in UI to satisfy tests without changing the tests unless the contract deliberately evolves.

## 5) Performance Optimizations

### Precomputed Assets Factors (Major Speed Improvement)
- **Problem Solved**: Geospatial operations (asset-hazard mapping) were taking 5+ minutes per test
- **Approach**: Speed optimization through hazard aggregation in `load_hazards()` function
- **Implementation**: `load_hazards()` includes aggregation_factor parameter to reduce raster resolution for faster processing
- **Result**: Maintains accuracy while significantly improving performance
- **Benefits**: No caching complexity, direct pipeline execution, easier to maintain

## 6) Data Setup Documentation

- **HAZARD_DATA_SETUP.md**: Complete guide for developers on setting up hazard data workspace structure and running the Brazil extraction pipeline
- **Workspace Structure**: `workspace/hazards_world/{hazard_type}/{scenario}.tif` organization
- **Processing Pipeline**: `data-raw/process_flood_maps_brazil.R` script for generating Brazil-specific subsets
- **Integration**: Automatic discovery and loading through `load_hazards()` function

## 7) Open questions / decisions
- Nearest-integer match or floor/ceil for hazard_intensity mapping
- CRS standardization for polygons and rasters
- How to handle assets lacking any location info
