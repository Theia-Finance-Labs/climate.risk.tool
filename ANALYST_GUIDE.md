# Climate Risk Tool – Analyst Documentation

## Overview

This document explains how hazard data, damage/cost factors, and financial logic are organised within the Climate Risk Tool. It is intended for analysts who work with the tool's inputs and outputs and who want to understand how the underlying modules interact.

---

## 1. Architecture Overview

The Climate Risk Tool consists of:

- **App entry points**: Start and coordinate the Shiny interface
  - `run_app.R`: Wrapper that starts the Shiny web application
  - `app_ui.R`: Defines the browser interface (tabs, input controls, tables, plots)
  - `app_server.R`: Connects the UI to the analysis code

- **Orchestration function**: Runs the complete climate-risk pipeline
  - `compute_risk.R`: Core function executing the entire pipeline from reading assets and hazards to producing company-level risk metrics

- **Configuration modules**: Define hazard types, scenario metadata, and application parameters
  - `app_config.R`: App-level configuration (paths, defaults)
  - `config__hazard_types.R`: Defines which indicators each hazard type uses and which is "primary" for the UI

- **Domain modules**: Geospatial extraction, hazard processing, damage/cost factor matching, and financial modelling
  - `geospatial__*.R`: Assign hazards to locations and match with damage/cost factors
  - `assets__*.R`: Build baseline and shocked trajectories at asset level
  - `shock__*.R`: Apply acute climate shocks to revenue and profit
  - `companies__*.R`: Aggregate asset-level results to company-level financials
  - `utils__*.R`: Reusable helpers (loading hazards, reading inputs, validation, result formatting)

- **UI modules**: The Shiny interface
  - `mod_*.R`: Individual modules for control panels, results display, and visualisation

The execution pipeline is driven by `compute_risk.R`, which loads data, processes assets and hazards, applies damage factors, and produces asset-level and company-level results.

---

## 2. Hazard Catalogue

**File:** `R/config__hazard_types.R`

**Function:** `get_hazard_type_config()`

This module defines the list of hazard types supported by the tool.

For each hazard type, it specifies:

- **Indicators used in the analysis**:
  - `Fire`: `land_cover`, `FWI`, `days_danger_total`
  - `Flood`: `depth(cm)`
  - `Heat`: `HI`
  - `Drought`: `SPI3`
- **Primary indicator**: Used for scenario/return-period selection in the interface
- **Short description**: Used for UI information displays

This configuration determines which hazard indicators must be present in the hazard inventory and how hazards are grouped in the UI and downstream logic.

---

## 3. Hazard Inventory and Data Loading

**File:** `R/utils__load_hazards.R`

**Function:** `load_hazards_and_inventory(hazards_dir, aggregate_factor = 1L)`

This function constructs a complete inventory of available hazard layers and loads the corresponding data files.

**Steps performed:**

1. Reads `hazards_metadata.csv`, which contains:
   - Hazard type
   - Indicator
   - Scenario name
   - Return period
   - File name
   - Additional metadata fields

2. Validates metadata and constructs a clean hazard inventory table

3. Loads the hazard data for each row in the inventory:
   - **GeoTIFF (`.tif`)**
   - **NetCDF (`.nc`)**
   - **CSV (`.csv` point sets)**

4. Returns:
   - `hazards`: A named list of raster or table objects
   - `inventory`: A tidy metadata table referenced throughout the pipeline

The inventory defines the universe of hazard–scenario–return period combinations available to the app.

---

## 4. Event–Hazard Mapping

**File:** `R/utils__create_event_hazard_mapping.R`

**Function:** `create_event_hazard_mapping(events, hazards_inventory, aggregation_method)`

This module translates user-defined events (hazard type, scenario, return period, year) into the specific hazard layers that must be extracted.

**Key points:**

- For **single-indicator hazards** (Flood, Heat, Drought), each event corresponds to one hazard layer
- For **multi-indicator hazards** (notably Fire), one user event produces several internal hazard layers:
  - `land_cover` (static)
  - `FWI` (scenario-dependent)
  - `days_danger_total` (scenario-dependent)

All hazards linked to the same event share a unique `event_id` and the selected `event_year`.

This mapping allows downstream modules to treat multi-indicator hazards consistently.

---

## 5. Hazard Extraction for Assets

**File:** `R/geospatial__extract_hazard_statistics.R`

**Function:** `extract_hazard_statistics(assets_df, hazards, hazards_inventory, precomputed_hazards, aggregation_method, damage_factors_df)`

This module assigns hazard intensities to assets using:

- **Coordinate-based extraction**, when latitude/longitude are available
- **Precomputed administrative-unit hazards**, when coordinates are not available

### 5.1 Coordinate-Based Extraction

Asset geometries are created using small polygons (`geospatial__create_asset_geometries.R`).

For each hazard layer:
- Raster is cropped and masked to the asset polygon
- Pixel values are aggregated based on the selected method (mean, median, pXX, max, etc.)
- Special handling: Fire `land_cover` uses **mode**, not mean

**Outputs include:**
- Asset identifiers
- Hazard type, indicator, scenario, return period
- Extracted `hazard_intensity`
- `matching_method = "coordinates"`

### 5.2 Administrative-Unit Extraction

If coordinates are missing, the tool uses:
- `municipality`
- `state`

to match against `precomputed_adm_hazards.csv`.

Outputs use the same schema as coordinate extraction, with `matching_method = "precomputed"`.

Both extraction paths produce a unified table used for damage/cost factor matching.

---

## 6. Matching Damage and Cost Factors

**File:** `R/geospatial__join_damage_cost_factors.R`

**Function:** `join_damage_cost_factors(assets_with_hazards, damage_factors_df, cnae_exposure = NULL, land_cover_legend = NULL)`

This module assigns damage and cost factors to asset–hazard combinations.

It dispatches to hazard-specific matching functions:
- `join_flood_damage_factors()`
- `join_compound_damage_factors()` (Heat)
- `join_drought_damage_factors()`
- `join_fire_damage_factors()`

It returns a harmonised table containing:
- Hazard intensities
- `damage_factor`
- `cost_factor`
- Any additional fields (e.g., `business_disruption`)

The following subsections summarise the logic for each hazard.

### 6.1 Flood Damage/Cost Factors

**Function:** `join_flood_damage_factors(flood_assets, damage_factors_df)`

**Matching rules:**

1. Subset the damage table to:
   - `hazard_type = "Flood"`
   - Matching `hazard_indicator`
   - Matching `asset_category`

2. Match each asset's flood depth to the **closest intensity** in the damage table

3. If depth exceeds the maximum available, use the highest value in the table

**Outputs include:**
- `damage_factor`
- `cost_factor`
- `business_disruption` (if provided)

This is a nearest-value lookup, not an interpolation.

### 6.2 Heat Damage Factors

**Function:** `join_compound_damage_factors(compound_assets, damage_factors_df, cnae_exposure = NULL)`

**Matching uses the following variables:**
- State (province)
- Scenario / GWL (global warming level as proxy)
- Exposure metric derived from:
  - CNAE sector when available
  - Asset category fallback otherwise

Heat modelling uses only `damage_factor`. `cost_factor` and `business_disruption` are set to NA.

### 6.3 Drought Damage Factors

**Function:** `join_drought_damage_factors(drought_assets, damage_factors_df)`

**Matching logic includes:**
- Crop subtype
- Province
- Season
- SPI3 intensity

**Rules:**
1. Try exact match: province + crop subtype
2. Fallback to any province with that crop
3. If crop subtype unsupported, fallback to "Other" category
4. Intensity is matched to the nearest value in the table
5. SPI3 may be capped at lower bounds depending on dataset conventions

Crops with multiple seasons use either season-specific damage or averaged values outside the window.

### 6.4 Fire Damage/Cost Factors

**Function:** `join_fire_damage_factors(fire_assets, damage_factors_df, land_cover_legend)`

Fire impacts use a combination of:
- Land cover class (from land cover hazard layer)
- Fire Weather Index (FWI)
- Danger days
- Optional land-cover risk legend

This function merges these inputs with the fire section of the damage table and assigns damage and cost factors accordingly.

---

## 7. Financial Trajectory Pipeline

The financial pipeline transforms hazard intensities and damage factors into monetary impacts through a sequence of trajectory computations.

### 7.1 Pipeline Overview

The trajectory pipeline follows this sequence:

1. **Baseline Trajectories** → Create no-shock projections
2. **Shock Application** → Apply climate impacts
3. **Scenario Concatenation** → Combine baseline and shocked scenarios
4. **Discounting** → Apply present value calculations
5. **Company Aggregation** → Roll up to company level
6. **Risk Metrics** → Compute NPV, PD, Expected Loss

### 7.2 Baseline Trajectory Construction

**File:** `R/assets__compute_baseline_trajectories.R`

**Function:** `compute_baseline_trajectories(baseline_assets, companies, growth_rate = 0.02, start_year = 2025, end_year = 2050)`

This function creates baseline (no-shock) yearly trajectories for all assets.

**Process:**

1. **Revenue allocation** (`compute_yearly_baseline_revenue()`):
   - Allocates company revenue to assets based on `share_of_economic_activity`
   - Projects forward using `growth_rate` (default 2% annually)
   - Formula: `revenue(t) = revenue(t-1) × (1 + growth_rate)`

2. **Profit computation** (`compute_profits_from_revenue()`):
   - Applies company-specific `net_profit_margin` to revenue
   - Formula: `profit = revenue × net_profit_margin`

**Output:**
- Tibble with columns: `asset`, `company`, `year`, `revenue`, `profit`
- One row per asset per year from `start_year` to `end_year`

### 7.3 Shock Application Sequence

**File:** `R/shock__compute_shock_trajectories.R`

**Function:** `compute_shock_trajectories(yearly_baseline_profits, assets_with_factors, events, companies, start_year = 2025)`

This orchestrator applies climate shocks to baseline trajectories in a precise sequence.

**Shock sequence:**

1. **Start with baseline revenue** (drop profit temporarily)
2. **Apply acute revenue shocks** → `apply_acute_revenue_shock()`
3. **Recompute profits** from shocked revenue → `compute_profits_from_revenue()`
4. **Apply acute profit shocks** → `apply_acute_profit_shock()`

This sequence ensures that:
- Revenue shocks affect the revenue base used for profit calculation
- Profit shocks (e.g., building damage costs) are applied after profit recomputation

**Output:**
- Tibble with columns: `asset`, `company`, `year`, `revenue`, `profit`
- Same structure as baseline, but with shocks applied

---

## 8. Detailed Shock Mechanisms

### 8.1 Revenue Shock Application

**File:** `R/shock__apply_acute_revenue_shock.R`

**Function:** `apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)`

This function applies revenue shocks from acute climate events. Multiple shocks in the same year are applied sequentially by `event_id`.

**Hazard-specific logic:**

#### Flood Revenue Shocks

**Function:** `apply_flood_shock()`

**For non-agriculture assets** (commercial building, industrial building):
- Uses `business_disruption` (days)
- Formula: `revenue_shocked = revenue × (1 - disruption_days / 365)`

**For agriculture assets:**
- Applies **both** damage factor and business disruption sequentially:
  1. Apply damage: `revenue_1 = revenue × (1 - damage_factor)`
  2. Apply disruption: `revenue_shocked = revenue_1 × (1 - disruption_days / 365)`
- Ensures revenue ≥ 0

#### Heat Revenue Shocks

**Function:** `apply_compound_shock()`

Uses **Cobb-Douglas production function** to model labor productivity loss:

**Parameters (hardcoded from CD_inputs.csv):**
- `L0 = 339.2285` (baseline labor)
- `K0 = 87025023` (baseline capital)
- `E0 = 43.99034` (baseline energy)
- `lnA = 2.398` (technology parameter)
- `B1 = 0.602` (capital elasticity)
- `B2 = 0.455` (labor elasticity)
- `B3 = 0.147` (energy elasticity)

**Process:**

1. Calculate baseline output:
   ```
   Y_base = exp(lnA + B1×ln(K0) + B2×ln(L0) + B3×ln(E0))
   ```

2. Calculate weighted labor productivity loss:
   ```
   weighted_lp_loss = (hazard_intensity / 365) × damage_factor
   ```
   - `hazard_intensity` = days with extreme heat
   - `damage_factor` = raw labor productivity loss (negative value from damage table)

3. Adjust labor input:
   ```
   L_adjusted = L0 × (1 + weighted_lp_loss)
   ```
   Note: `weighted_lp_loss` is negative, so this reduces L

4. Calculate shocked output:
   ```
   Y_shock = exp(lnA + B1×ln(K0) + B2×ln(L_adjusted) + B3×ln(E0))
   ```

5. Calculate relative change:
   ```
   change = (Y_shock / Y_base) - 1
   ```

6. Apply to revenue:
   ```
   revenue_shocked = revenue × (1 + change)
   ```

#### Drought Revenue Shocks

**Function:** `apply_drought_shock()`

**Applies only to agriculture assets.**

**Process:**
1. Aggregate damage factors by asset (average if multiple rows)
2. Apply damage: `revenue_shocked = revenue × (1 - damage_factor)`
3. Ensure revenue ≥ 0

#### Fire Revenue Shocks

**Function:** `apply_fire_revenue_shock()`

**Applies only to agriculture assets.**

Uses the **full fire damage formula**:
```
fire_damage_pct = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365)
```

**Components:**
- `land_cover_risk`: Risk multiplier from land cover category
- `damage_factor(FWI)`: Damage factor based on Fire Weather Index
- `days_danger_total / 365`: Fraction of year with dangerous fire conditions

**Application:**
```
revenue_shocked = revenue × (1 - fire_damage_pct)
```

### 8.2 Profit Shock Application

**File:** `R/shock__apply_acute_profit_shock.R`

**Function:** `apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)`

This function applies direct profit shocks (typically capital damage costs) after revenue shocks and profit recomputation.

**Hazard-specific logic:**

#### Flood Profit Shocks

**Applies only to commercial building and industrial building** (NOT agriculture).

**Formula:**
```
acute_damage = damage_factor × cost_factor
```

**Application:**
```
profit_shocked = profit - acute_damage
```

This represents the cost of repairing or replacing damaged buildings.

#### Fire Profit Shocks

**Applies only to commercial building and industrial building** (NOT agriculture).

Uses the **full fire damage formula** with cost factor:
```
acute_damage = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365) × cost_factor
```

**Application:**
```
profit_shocked = profit - acute_damage
```

This represents the cost of building destruction from fire.

#### Other Hazards

- **Drought**: No profit shock (only revenue shock for agriculture)
- **Heat**: No profit shock (only revenue shock via labor productivity)

**Multiple events in same year:**
If multiple events affect the same asset-year, acute damages are summed before application.

### 8.3 Scenario Concatenation

**File:** `R/utils__concatenate_baseline_and_shock.R`

**Function:** `concatenate_baseline_and_shock(yearly_baseline_df, yearly_shocked_df)`

This function combines baseline and shocked trajectories into a single dataframe with a `scenario` column.

**Process:**

1. Add `scenario = "baseline"` to baseline trajectories
2. Add `scenario = "shock"` to shocked trajectories
3. Bind rows and arrange by `asset`, `scenario`, `year`

**Output:**
- Tibble with columns: `asset`, `company`, `year`, `scenario`, `revenue`, `profit`
- Twice as many rows as input (one set for baseline, one for shock)

This unified format enables downstream comparison and NPV calculations across scenarios.

---

## 9. Company Aggregation and Risk Metrics

**File:** `R/companies__compute_financials.R`

After trajectory construction, the pipeline aggregates to company level and computes risk metrics.

**Steps:**

1. **Discounting** (`discount_yearly_profits()`):
   - Applies present value discounting to future profits
   - Formula: `PV(t) = profit(t) / (1 + discount_rate)^(t - start_year)`

2. **Company aggregation** (`aggregate_assets_to_company()`):
   - Sums discounted profits across all assets for each company
   - Groups by `company`, `scenario`, `year`

3. **NPV calculation**:
   - Sums all discounted profits over the projection period
   - Computed separately for baseline and shock scenarios

4. **Probability of Default (PD)** using Merton model:
   - Based on company equity, debt, and NPV difference
   - Only computed for shock scenarios

5. **Expected Loss (EL)**:
   - Formula: `EL = LGD × Loan_Size × PD`
   - Where LGD (Loss Given Default) is typically assumed

6. **Result formatting** (`gather_and_pivot_results()`):
   - Pivots to wide format for reporting
   - One row per company with baseline and shock metrics side-by-side

---

## 10. Shiny Application Modules

The Shiny interface provides entry points to configure inputs, run the analysis, and view results.

### Control & Input Selection

- **`mod_control.R`**: Base directory selection, event selection, analysis trigger
- **`mod_hazards_events.R`**: Selection of hazard events (scenarios, return periods, years)

### Results Views

- **`mod_results_assets.R`**: Asset-level tables and plots
- **`mod_results_companies.R`**: Aggregated company-level results
- **`mod_company_analysis.R`**: Detailed company drill-down
- **`mod_profit_pathways.R`**: Time-series visualisation of baseline and shocked profits
- **`mod_status.R`**: Run status and errors

The Shiny modules present the output of `compute_risk()`. No recalculation occurs inside UI modules.

---

## 11. Quick Reference Guide

| **Topic** | **Relevant File** |
|-----------|------------------|
| Hazard types, indicators, definitions | `R/config__hazard_types.R` |
| Hazard inventory & data loading | `R/utils__load_hazards.R` |
| Event → hazard mapping | `R/utils__create_event_hazard_mapping.R` |
| Coordinate & administrative hazard extraction | `R/geospatial__extract_hazard_statistics.R` |
| Damage & cost factors matching | `R/geospatial__join_damage_cost_factors.R` |
| Flood matching | `join_flood_damage_factors()` in `R/geospatial__join_damage_cost_factors.R` |
| Heat matching | `join_compound_damage_factors()` in `R/geospatial__join_damage_cost_factors.R` |
| Drought matching | `join_drought_damage_factors()` in `R/geospatial__join_damage_cost_factors.R` |
| Fire matching | `join_fire_damage_factors()` in `R/geospatial__join_damage_cost_factors.R` |
| Baseline trajectories | `R/assets__compute_baseline_trajectories.R` |
| Shock orchestration | `R/shock__compute_shock_trajectories.R` |
| Revenue shocks (all hazards) | `R/shock__apply_acute_revenue_shock.R` |
| Profit shocks (Flood, Fire) | `R/shock__apply_acute_profit_shock.R` |
| Scenario concatenation | `R/utils__concatenate_baseline_and_shock.R` |
| Financial computations | `R/compute_risk.R`, `R/companies__compute_financials.R` |

---

## 12. End-to-End Pipeline Summary

1. **Hazard catalogue** defines available hazard types and indicators
2. **Hazard inventory** loads corresponding datasets
3. **Assets** receive hazard intensities from rasters or precomputed ADM layers
4. **Events** map scenario selections to hazard layers
5. **Hazard intensities** are matched to damage and cost factors
6. **Baseline trajectories** are built (revenue and profit projections)
7. **Revenue shocks** are applied (hazard-specific formulas)
8. **Profits** are recomputed from shocked revenue
9. **Profit shocks** are applied (capital damage costs)
10. **Scenarios** are concatenated (baseline + shock)
11. **Discounting** is applied to future cash flows
12. **Company-level metrics** are generated (NPV, PD, Expected Loss)
13. **Results** flow to the Shiny interface

This structure ensures a transparent and traceable path from raw hazard datasets to final risk metrics.
