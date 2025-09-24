# Climate Risk Tool - Working Document

Purpose: living reference for code structure, data schemas, and test plan. Keep this updated when adding functions or tests.

## High-level pipeline

1. Read inputs: assets and companies
2. Geolocate assets (priority: geoloc > municipality > province). Output adds `geometry` (polygon) and `centroid` columns
3. Raster cutouts for each hazard over asset polygon; add columns per hazard
4. Summarize hazard cutouts into average intensity per hazard
5. Join damage and cost factors by nearest `hazard_intensity` (integer) and `asset_category`
6. Apply acute shock (placeholder; depends on `shock_year`)
7. Apply chronic shock (placeholder; no parameter)
8. Compute asset impact: update `share_of_economic_activity`; drop working columns
9. Build scenarios: concat baseline vs shock
10. Asset revenue (placeholder) using company data and `growth_rate`
11. Asset profits using `net_profit_margin`
12. Discounted net profits (placeholder)
13. Company NPV by scenario (aggregate of assets)
14. Company PD via Merton (placeholder) by scenario
15. Expected Loss by scenario
16. Gather results and pivot for reporting

## Data locations (tests/test_data)
- user_input: `asset_information.csv`, `company.csv`
- areas: province GeoJSON (ADM1), municipality GeoJSON (ADM2)
- hazards: .tif rasters
- damage_and_cost_factors.csv

## Function contracts (implemented via TDD)

### Main Orchestrator Function
- ✅ **core_compute_risk(base_dir, shock_year, growth_rate=0.02, net_profit_margin=0.1, discount_rate=0.05, verbose=TRUE)** -> list(assets, companies, intermediate) - **MAIN FUNCTION** that executes the complete 18-step climate risk analysis pipeline from raw inputs to final risk metrics. Serves as the primary entry point and documentation center for the entire workflow.

### Individual Pipeline Functions  
- ✅ read_inputs(base_dir) -> list(assets, companies) - reads CSV files, converts to snake_case, parses numeric columns
- ✅ load_hazards(hazards_dir) -> named list of SpatRaster objects from .tif files
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

## Testing strategy
- Snapshot representative rows after each stage
- Validate column presence, types, and row counts; minimal numeric checks
- Use deterministic seeds and small polygons

## Open questions / decisions
- Nearest-integer match or floor/ceil for hazard_intensity mapping
- CRS standardization for polygons and rasters
- How to handle assets lacking any location info
