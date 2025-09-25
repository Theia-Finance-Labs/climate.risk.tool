# Climate Risk Tool - Working Document

Purpose: living reference for code structure, data schemas, and test plan. Keep this updated when adding functions or tests.

## 1) Shiny Application (UI/Server)

- `app_ui()`/`app_server()` orchestrate modules.
- Module: `mod_hazards_events_ui/server`
  - UI elements: Hazard select, Scenario select (dependent), Chronic checkbox, optional Shock year, Add hazard button, Configured events table.
  - Output: stored events with fields `event_id`, `hazard_type`, `scenario`, `event_year` (NA if chronic), `chronic`.
- Startup: hazards inventory loads immediately from `golem::get_golem_options("base_dir")` and populates the dropdowns.

## 2) Core Split Pipeline (Final)

- ✅ `compute_risk(assets, companies, hazards, areas, damage_factors, events, growth_rate, net_profit_margin, discount_rate, verbose)` → list(assets, companies)
  - Orchestrates: `compute_hazard_events()` → combine per-asset across events (min share rule) → `build_scenarios()` → `compute_financials_from_assets()`.
- ✅ `compute_hazard_events(assets, hazards, areas, events, damage_factors)` → long table [asset, company, event_id, hazard_type, scenario, event_year, chronic, share_of_economic_activity].
- ✅ `compute_financials_from_assets(assets_scenarios, companies, growth_rate, net_profit_margin, discount_rate)` → list(assets, companies).

Event combination rule: worst-case per asset (min share). Configurable later.

## 3) Supporting Core Functions

- user_input: `asset_information.csv`, `company.csv`
- areas: province GeoJSON (ADM1), municipality GeoJSON (ADM2)
- hazards: .tif rasters
- damage_and_cost_factors.csv

## Function contracts (implemented via TDD)

### Workflow in app
1. User configures events via module (can add many). Stored list drives analysis.
2. Compute per-event asset impacts with `compute_hazard_events`.
3. Combine events to shocked asset shares (current rule: min share per asset across events; configurable).
4. Build baseline vs shock scenarios and run `compute_financials_from_assets`.

### Individual Pipeline Functions  
- ✅ read_assets(base_dir) -> data.frame - reads asset CSV file from base_dir/user_input/, converts to snake_case, parses numeric columns
- ✅ read_companies(file_path) -> data.frame - reads company CSV file from specified path, converts to snake_case, parses numeric columns
- ✅ load_hazards(hazards_dir) -> named list of SpatRaster objects from .tif files; searches recursively in hazard-type subfolders (e.g., `floods/`, `heat/`) and names layers as `hazardType__scenario` based on subfolder and filename (sans extension)
- ✅ load_location_areas(municipalities_dir, provinces_dir) -> list(municipalities, provinces) - loads both area types at once
- ✅ load_municipalities(municipalities_dir) -> named list of sf objects from .geojson files
- ✅ load_provinces(provinces_dir) -> named list of sf objects from .geojson files  
- ✅ geolocate_assets(assets, hazards, municipalities_areas, provinces_areas) -> assets_with_geometry - adds geometry and centroid columns using lat/lon > municipality > province priority, uses size_in_m2 for lat/lon buffer sizing, now includes geolocation_method column
- ✅ cutout_hazards(assets_with_geometry, hazards) -> assets_with_hazard_values - extracts hazard raster values for each asset geometry, optimized to group by municipality/province for efficiency
- ✅ summarize_hazards(assets_with_hazard_values) -> assets_with_hazard_means - creates mean summary columns for each hazard
- ✅ join_damage_cost_factors(assets_with_hazard_means, factors_csv) -> assets_with_factors - maps hazard intensity to damage/cost factors by nearest integer and asset_category
- ✅ apply_acute_shock(df, shock_year) -> df + `acute_shock` - calculates sudden climate event impacts based on hazard intensity and shock year
- ✅ apply_chronic_shock(df) -> df + `chronic_shock` - calculates gradual climate change impacts focusing on temperature and precipitation trends
- ✅ compute_asset_impact(df) -> df with updated `share_of_economic_activity` - combines all impact factors and removes working columns
- ✅ build_scenarios(original_assets, shocked_assets) -> assets_long_scenarios - concatenates baseline and shock data with ordered factor scenario column
- ✅ compute_asset_revenue(df_assets, df_companies, growth_rate) -> df_assets_rev - allocates company revenue to assets based on share_of_economic_activity and growth_rate
- ✅ compute_asset_profits(df_assets_rev, net_profit_margin) -> df_assets_profit - multiplies asset revenue by net profit margin
- ✅ discount_net_profits(df_assets_profit, discount_rate) -> df_assets_dnp - applies present value discounting using company-specific terms where available
- ✅ compute_company_npv(df_assets_dnp) -> df_companies_npv - aggregates asset discounted profits to company-level NPV by scenario
- ✅ compute_company_pd_merton(df_companies_npv) -> df_companies_pd - calculates probability of default using simplified Merton model with company debt and volatility
- ✅ compute_expected_loss(df_companies_pd) -> df_companies_el - computes expected loss using EL = LGD * Loan_Size * PD formula
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

## 5) Open questions / decisions
- Nearest-integer match or floor/ceil for hazard_intensity mapping
- CRS standardization for polygons and rasters
- How to handle assets lacking any location info
