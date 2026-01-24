# climate.risk.tool

<!-- badges: start -->
<!-- badges: end -->

R package for climate risk assessment using geospatial hazard data and financial modeling. Supports GeoTIFF (.tif), NetCDF (.nc), and CSV hazard data formats with automated ensemble statistics extraction and multi-format pipeline handling.

## Installation

You can install the development version of climate.risk.tool from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Theia-Finance-Labs/climate.risk.tool")
```

## Usage

Once the package is installed, you can use it in two ways:

### 1. Programmatic Analysis

Load your data and run the complete climate risk analysis:

``` r
library(climate.risk.tool)

# Path to your base data directory. It must contain:
# - damage_and_cost_factors.csv
# - hazards/precomputed_adm_hazards.csv (precomputed hazard statistics for regions)
# - hazards/indicators/<indicator_folder>/metadata.csv (metadata for TIF hazards)
# - hazards/indicators/ directory with .tif, .nc, or .csv indicator files
# - hazards/config/ directory with <HazardName>.yml files
# - hazards/mappings/ directory with mapping tables (CSV/XLSX)
base_dir <- "/path/to/your/data"

# Path to your input folder containing:
# - asset_information.xlsx
# - company_information.xlsx
input_folder <- "/path/to/your/input_folder"

# Load all required data
assets <- read_assets(input_folder)
companies <- read_companies(input_folder)

# Load hazards with unified loader (supports TIF, NetCDF, and CSV formats)
hazard_data <- load_hazards_and_inventory(
  hazards_dir = file.path(base_dir, "hazards", "config"),
  hazard_indicators_dir = file.path(base_dir, "hazards", "indicators"),
  aggregate_factor = 16L
)
# Include all hazard sources (TIF, NC, CSV). Heat hazards are typically provided via CSV.
hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc, hazard_data$hazards$csv)
hazards_inventory <- hazard_data$inventory

precomputed_hazards <- read_precomputed_hazards(base_dir)
damage_factors <- read_damage_cost_factors(base_dir)

# Optional: Load additional data for specific hazard types
cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)  # For Heat hazards
land_cover_legend <- read_land_cover_legend(base_dir)  # For Fire hazards

# Create events data frame with required columns
# Hazard names use the format: {HazardType}__{indicator}__GWL={scenario}__RP={return_period}__ensemble={variant}__season={season}
events <- data.frame(
  hazard_type = c("Flood", "Heat", "Drought", "Fire"),
  hazard_name = c(
    "Flood__depth(cm)__GWL=present__RP=100",
    "Heat__HI__GWL=2__RP=10__ensemble=median",
    "Drought__SPI3__GWL=1.5__RP=10__season=Summer__ensemble=median",
    "Fire__FWI__GWL=3__RP=50__ensemble=median"
  ),
  scenario_name = c("present", "2", "1.5", "3"),
  scenario_code = c("present", "2", "1.5", "3"),
  hazard_return_period = c(100, 10, 10, 50),
  event_year = c(2030L, 2035L, 2032L, 2030L),
  stringsAsFactors = FALSE
)
# Note: event_id is auto-generated if not provided

# Run the complete climate risk analysis
results <- compute_risk(
  assets = assets,
  companies = companies,
  events = events,
  hazards = hazards,
  hazards_inventory = hazards_inventory,
  precomputed_hazards = precomputed_hazards,
  damage_factors = damage_factors,
  cnae_exposure = cnae_exposure,  # Optional
  land_cover_legend = land_cover_legend,  # Optional
  growth_rate = 0.02,
  discount_rate = 0.05,
  risk_free_rate = 0.02,
  aggregation_method = "mean"  # Options: "mean", "median", "p10", "p90", etc.
)

# Access results
results$assets_factors   # Asset-level hazard exposure with damage factors and event information
results$companies        # Company NPV, PD, and Expected Loss by scenario (aggregated)
results$assets_yearly    # Detailed yearly asset trajectories with revenue, profit, and discounted values
results$companies_yearly # Detailed yearly company trajectories with aggregated financials
``` 

### 2. Interactive Shiny Application

Launch the web interface for interactive analysis:

``` r
library(climate.risk.tool)

# Path to your base data directory (containing hazards, damage factors, etc.)
base_dir <- "/path/to/your/data"

run_app(base_dir = base_dir)
```

This will open the climate.risk.tool interface, where you can:
1. Select a folder containing `asset_information.xlsx` and `company_information.xlsx` files
2. Configure hazard events
3. Run the risk calculations
4. View and download results interactively

The app uses a native folder browser dialog for easy folder selection.

## Developer Setup

If you're contributing to this package, follow these steps:

### 1. Install Development Dependencies

``` r
# Install the package with all suggested dependencies (including shinytest2)
pak::pak("Theia-Finance-Labs/climate.risk.tool", dependencies = TRUE)

# Or install development dependencies manually
install.packages(c("devtools", "testthat", "shinytest2", "knitr", "rmarkdown"))
```

### 2. Development Workflow

Load all functions without reinstalling:
``` r
devtools::load_all(compile = FALSE)  # compile = FALSE since src/ contains Python code, not C/C++
```

Run the development app with hot reloading:
``` r
# Set environment variable for test data (optional)
Sys.setenv(CLIMATE_RISK_BASE_DIR = "tests/tests_data")

# Run development version
golem::run_dev()
```

### 3. Testing

Run all tests:
``` r
devtools::test()
```

Run specific test files:
``` r
devtools::test_file("tests/testthat/test-app_ui.R")
devtools::test_file("tests/testthat/test-app_server.R")
devtools::test_file("tests/testthat/test-app_e2e.R")  # End-to-end tests
```

### 4. Documentation and Package Checks

Update function documentation:
``` r
devtools::document()   # Updates man/ files from roxygen2 comments
```

Run full package checks:
``` r
devtools::check()      # Full R CMD CHECK
```

### 5. Hazard Data Setup

For setting up hazard data and running the Brazil extraction pipeline, see:

**[HAZARD_DATA_SETUP.md](HAZARD_DATA_SETUP.md)** - Complete guide for developers

Quick reference:
- Add hazard configs to `hazards/config/<HazardName>.yml`
- Place mapping tables in `hazards/mappings/`
- Place hazard indicators in appropriate format:
  - TIF files: `hazards/indicators/<indicator_folder>/*.tif` with `metadata.csv` in the same folder
  - NetCDF files: `hazards/indicators/*.nc` (auto-discovered)
  - CSV files: `hazards/indicators/*.csv` (auto-discovered)
- Run `Rscript data-raw/process_flood_maps_brazil.R` to generate Brazil subsets (if applicable)
- Processed files are saved to `tests/tests_data/hazards/`

### 5bis. Building NetCDF hazard indicators from `workspace/hazards/indicators/` (Python)

This repo includes a small Python utility that converts a local “indicator folder” layout
(`workspace/hazards/indicators/`) into a NetCDF-only layout that is **fast to lazy-load**
and **optimized for runtime polygon extraction** in the app.

#### Input folder layout

The input root is expected to look like:

- `workspace/hazards/indicators/`
  - `<indicator_name_1>/` (contains either `*.tif`/`*.tiff` + `metadata.csv`, OR a `*.nc`)
  - `<indicator_name_2>/` (contains either `*.tif`/`*.tiff` + `metadata.csv`, OR a `*.nc`)
  - ...

Notes:
- If a folder contains GeoTIFFs, the script will convert them to a single NetCDF using the `metadata.csv` in that folder.
- If a folder contains a NetCDF, the script will copy it (and rewrite only if it needs to rename the variable / enforce chunking+compression).
- CSV-backed indicators are intentionally ignored by this script.

#### `metadata.csv`

If you have GeoTIFF indicators, provide `metadata.csv` in each indicator folder.

Required columns:
- `hazard_file`
- `hazard_type`
- `hazard_indicator`
- `gwl`
- `return_period`

The script matches `hazard_file` to the TIFF filenames found in each indicator folder and uses:
- unique `gwl` values → NetCDF dimension `GWL`
- unique `return_period` values → NetCDF dimension `return_period`

#### Output folder layout

The output root will contain one NetCDF per indicator:

- `workspace/demo_inputs_refacto/hazards/indicators/`
  - `flood_depth.nc`
  - `land_cover.nc`
  - `fire_weather_index.nc`
  - ...

Each file is written so the app can lazy-load quickly at startup and then read efficiently at runtime.

#### How to run (full workflow)

```bash
python3 src/climate_risk_tool_python/netcdf_mgmt/build_hazard_indicators_refacto.py \
  --input-root workspace/hazards/indicators \
  --output-root workspace/demo_inputs_refacto/hazard_indicators \
  --overwrite
```

#### Performance-oriented NetCDF writing (automatic)

No tuning flags are needed: the writer is fully automated with the goal of making runtime extraction
as close as possible to the original GeoTIFF performance.

- **Chunking** (most important for runtime `crop()`/`mask()`):
  - Derived from the source GeoTIFF internal block size (COG tiling), which matches the access pattern of polygon extraction.
  - Categorical `uint8` (e.g. `land_cover`) uses 1× the TIFF block size.
  - Multi-byte numeric rasters (e.g. flood `uint32`) use 2× the TIFF block size.
- **Compression level**:
  - Chosen automatically (typically `4` for categorical `uint8`, `6` for multi-byte numeric), balancing size vs decompression cost.
- **Shuffle filter**:
  - Enabled only for multi-byte numeric types (it does not help `uint8`).
- **Progress display**:
  - TIFF → NetCDF writing shows a `tqdm` progress bar instead of printing thousands of “tile N” lines.

### 6. Package Structure

The codebase is organized into logical modules using a clear naming convention:

#### Core Modules in `R/` Directory

- **`assets__*.R`** - Asset-level calculations and transformations
  - Baseline trajectories, shock applications, yearly scenarios
  - Revenue and profit computations, discounting operations

- **`companies__*.R`** - Company-level financial analysis
  - NPV calculations, probability of default (Merton model)
  - Expected loss computations, financial aggregations

- **`geospatial__*.R`** - Geographic and hazard processing
  - Asset geolocation, hazard data loading and processing
  - Spatial operations, damage factor integration
  - Multi-format extraction workflows for TIF, NetCDF, and CSV sources

- **`utils__*.R`** - Utility functions and data I/O
  - Input data reading, hazard inventory management
  - Area loading, result gathering and formatting
  - `filter_hazards_by_events()` for smart hazard filtering
  - Enhanced `load_hazards_and_inventory()` supporting TIF, NetCDF, and CSV formats

- **`mod_*.R`** - Shiny application modules
  - UI/Server pairs for interactive components
  - Modular Shiny architecture following golem framework

- **`app_*.R`** - Main application components
  - App configuration, UI layout, server logic
  - Entry points for the Shiny application

- **`compute_risk.R`** - Main orchestration function
  - Coordinates the complete climate risk analysis pipeline
  - Integrates all modules for end-to-end processing

- **`run_app.R`** - Application launcher
  - Entry point for starting the Shiny application

#### Data Format Support

The package supports NetCDF (.nc) hazard data format:

**NetCDF (.nc) Files** - Scientific format with multi-dimensional data
- Auto-discovers from directory structure and file dimensions
- Uses terra-based lazy loading for efficient memory usage
- Loads 'mean' ensemble by default for each hazard scenario
- Naming format: `{HazardType}__{indicator}__GWL={level}__RP={period}__ensemble={variant}__season={season}`
- Example: `Drought__SPI3__GWL=1.5__RP=10__season=Summer__ensemble=mean`
- Supports multiple dimensions: GWL, return_period, ensemble, season
- Spatial extraction computes statistics from raster values using polygon-based extraction

#### NetCDF Pipeline Handling

The pipeline handles NetCDF files through several mechanisms:

**1. Unified Loading (`load_hazards_and_inventory()`)**
- Scans directory tree for NetCDF files
- Extracts metadata from file structure and NetCDF dimensions
- Returns list: `list(hazards = ..., inventory = ...)`
- Creates unified inventory with `source` column ("nc")

**2. Smart Event Filtering (`filter_hazards_by_events()`)**
- Filters hazards by event requirements
- **NC hazards**: Base name matching (mean ensemble loaded by default)
- Returns filtered list of SpatRaster objects

**3. Spatial Extraction Workflow (`extract_hazard_statistics()`)**
- **Polygon-based extraction**: Crop and mask rasters to asset geometries
- **Aggregation methods**: mean, median, max, min, p2_5, p5, p95, p97_5
- **Priority cascade**: Coordinates → Municipality (ADM2) → Province (ADM1) → Error
- **Unified output**: Consistent column structure for all hazards

**4. Combined Results Processing**
- All downstream functions work identically with mixed format results
- Damage factor joining, shock application, and financial calculations are format-agnostic
- Final results combine data from all formats seamlessly

#### Other Key Directories

- `tests/testthat/` - Unit and integration tests following TDD principles
- `tests/tests_data/` - Test data for development and testing
- `man/` - Auto-generated documentation (do not edit manually)
- `CONTEXT.md` - Development notes and function contracts

## Hazard Data Computation Methods

The climate risk tool offers multiple complementary approaches for computing hazard statistics:

### 1. Pre-computed Hazard Results (Applied for assets without lat/lon but regional information)

To generate the file of pre-computed results, use the pre-computation notebook:

**[`data-raw/notebooks/precompute_hazard_per_adm.ipynb`](data-raw/notebooks/precompute_hazard_per_adm.ipynb)**

This Python notebook:
- Processes hazard data (TIF, NetCDF, and CSV) against administrative boundaries (ADM1/ADM2)
- Pre-computes hazard statistics for each region
- Generates `precomputed_adm_hazards.csv` with regional aggregates
- Supports both current climate and future scenarios (present, GWL levels)
- Handles ensemble statistics (mean, median, percentiles) for NetCDF data
- Significantly speeds up analysis by avoiding repeated spatial computations

### 2. Asset-to-Map Matching (Real-time Computation)

**Asset Size Consideration:**
- **Primary method**: Uses the actual asset size in square meters (m²) from asset data
- **Fallback method**: Uses a default area value when asset size is unavailable
- **Spatial computation**: Extracts hazard statistics using `exactextractr` for pixel-level accuracy (TIF files)
- **Format support**: Works with GeoTIFF (spatial), NetCDF (pre-computed statistics), and CSV (tabular lookup)

**Matching Priority Cascade:**
1. **Coordinates** (lat/lon): Direct spatial extraction from raster data or lookup from CSV
2. **Municipality (ADM2)**: Pre-computed regional statistics lookup
3. **State (ADM1)**: Pre-computed regional statistics lookup
4. **Error**: If no matching method is available

**When to use each method:**
- **Pre-computed**: Large-scale analyses, repeated runs, performance-critical applications, assets without coordinates
- **Real-time matching**: Small datasets, maximum precision requirements, custom geometries, assets with coordinates

The tool seamlessly handles both approaches, allowing mixed usage in the same analysis pipeline. Assets with coordinates use spatial extraction, while assets with only regional information use pre-computed statistics.
