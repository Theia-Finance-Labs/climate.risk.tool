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
# - precomputed_adm_hazards.csv (precomputed hazard statistics for regions)
# - hazards_name_mapping.csv (metadata for TIF hazards, optional for NC/CSV)
# - hazards/[hazard_type]/ directory with .tif, .nc, or .csv files
base_dir <- "/path/to/your/data"

# Path to your input folder containing:
# - asset_information.xlsx
# - company_information.xlsx
input_folder <- "/path/to/your/input_folder"

# Load all required data
assets <- read_assets(input_folder)
companies <- read_companies(input_folder)

# Load hazards with unified loader (supports TIF, NetCDF, and CSV formats)
hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
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
    "Heat__HI__GWL=2__RP=10__ensemble=mean",
    "Drought__SPI3__GWL=1.5__RP=10__season=Summer__ensemble=mean",
    "Fire__FWI__GWL=3__RP=50__ensemble=mean"
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
devtools::load_all()
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
- Create `hazards/{hazard_type}/` directory structure
- Place hazard files in appropriate format:
  - TIF files: `{hazard_type}/*.tif` (requires `hazards_name_mapping.csv`)
  - NetCDF files: `{hazard_type}/*.nc` (auto-discovered)
  - CSV files: `{hazard_type}/*.csv` (auto-discovered)
- Run `Rscript data-raw/process_flood_maps_brazil.R` to generate Brazil subsets (if applicable)
- Processed files are saved to `tests/tests_data/hazards/`

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

The package supports three hazard data formats that can be used together in the same analysis:

**GeoTIFF (.tif) Files** - Traditional raster format
- Requires `hazards_name_mapping.csv` for metadata
- Spatial extraction computes statistics from pixel values using `exactextractr`
- Naming format: `{HazardType}__{indicator}__GWL={scenario}__RP={return_period}`
- Example: `Flood__depth(cm)__GWL=present__RP=100`

**NetCDF (.nc) Files** - Modern scientific format with pre-computed statistics
- Auto-discovers from directory structure and file dimensions
- Direct extraction of pre-computed ensemble statistics (mean, median, p10, p90)
- Naming format: `{HazardType}__{indicator}__GWL={level}__RP={period}__ensemble={variant}__season={season}`
- Example: `Drought__SPI3__GWL=1.5__RP=10__season=Summer__ensemble=mean`
- **No spatial computation needed** - statistics pre-computed in the NC file

**CSV Files** - Tabular format for point-based or aggregated data
- Typically used for Heat hazards and other non-spatial hazard data
- Auto-discovered from directory structure
- Direct data lookup without spatial computation
- Naming format: `{HazardType}__{indicator}__GWL={level}__RP={period}__ensemble={variant}`
- Example: `Heat__HI__GWL=2__RP=10__ensemble=mean`

#### Mixed Format Pipeline Handling

The pipeline seamlessly handles mixed TIF, NetCDF, and CSV formats through several mechanisms:

**1. Unified Loading (`load_hazards_and_inventory()`)**
- Loads all three formats in a single call
- Returns combined list: `list(hazards = list(tif = ..., nc = ..., csv = ...), inventory = ...)`
- Creates unified inventory with `source` column indicating data format
- TIF files are optional (if no mapping file exists, only NC/CSV files are loaded)

**2. Smart Event Filtering (`filter_hazards_by_events()`)**
- Filters hazards by event requirements across all formats
- **TIF hazards**: Exact name matching
- **NC hazards**: Automatic ensemble expansion (1 event → 4 ensemble variants)
- **CSV hazards**: Direct lookup by hazard name
- Returns single filtered list combining all formats

**3. Multi-Format Extraction Workflow (`extract_hazard_statistics()`)**
- **Detects format mix** and chooses appropriate extraction method per hazard
- **TIF sources**: Spatial computation using `exactextractr` for pixel statistics
- **NC sources**: Direct extraction of pre-computed ensemble statistics
- **CSV sources**: Direct data lookup from tabular format
- **Priority cascade**: Coordinates → Municipality (ADM2) → Province (ADM1) → Error
- **Unified output**: Same column structure regardless of source format

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
