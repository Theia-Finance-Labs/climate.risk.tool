# climate.risk.tool

<!-- badges: start -->
<!-- badges: end -->

R package for climate risk assessment using geospatial hazard data and financial modeling. Supports both GeoTIFF (.tif) and NetCDF (.nc) hazard data formats with automated ensemble statistics extraction.

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

# Path to your input data directory. It must contain:
# - user_input/: asset_information.csv, company.csv
# - damage_and_cost_factors.csv
# - precomputed_adm_hazards.csv (precomputed hazard statistics for regions)
# - hazards_name_mapping.csv (metadata for TIF hazards, optional for NC)
# - hazards/[hazard_type]/ directory with .tif or .nc files
base_dir <- "/path/to/your/data"

# Load all required data
assets <- read_assets(base_dir)
companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))

# Load hazards with unified loader (supports both TIF and NetCDF formats)
hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"))
hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)  # Flatten for compute_risk
hazards_inventory <- hazard_data$inventory

precomputed_hazards <- read_precomputed_hazards(base_dir)
damage_factors <- read_damage_cost_factors(base_dir)

# Create events (updated format)
events <- data.frame(
  hazard_type = "flood",
  hazard_name = "flood__rcp85_h100glob", 
  event_year = 2030,
  chronic = FALSE
)

# Run the complete climate risk analysis
results <- compute_risk(
  assets = assets,
  companies = companies,
  events = events,
  hazards = hazards,
  hazards_inventory = hazards_inventory,
  precomputed_hazards = precomputed_hazards,
  damage_factors = damage_factors,
  growth_rate = 0.02,
  net_profit_margin = 0.1,
  discount_rate = 0.05
)

# Access results
results$assets           # Aggregated asset NPV by scenario
results$companies        # Company NPV, PD, and Expected Loss by scenario
results$assets_yearly    # Detailed yearly asset trajectories
results$companies_yearly # Detailed yearly company trajectories
results$assets_factors   # Asset-level hazard exposure and damage factors
``` 

### 2. Interactive Shiny Application

Launch the web interface for interactive analysis:

``` r
library(climate.risk.tool)

# Path to your input data directory
base_dir <- "/path/to/your/data"

run_app(base_dir = base_dir)
```

This will open the climate.risk.tool interface, where you can upload company data,
run the risk calculations, and view the results interactively.

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
- Create `workspace/hazards_world/{hazard_type}/{scenario}.tif` structure
- Run `Rscript data-raw/process_flood_maps_brazil.R` to generate Brazil subsets
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
  - **New**: Dual extraction workflows for TIF vs NetCDF sources

- **`utils__*.R`** - Utility functions and data I/O
  - Input data reading, hazard inventory management
  - Area loading, result gathering and formatting
  - **New**: `filter_hazards_by_events()` for smart hazard filtering
  - **New**: Enhanced `load_hazards_and_inventory()` supporting both TIF and NetCDF

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

The package supports two hazard data formats that can be used together in the same analysis:

**GeoTIFF (.tif) Files** - Traditional raster format
- Requires `hazards_name_mapping.csv` for metadata
- Spatial extraction computes statistics from pixel values
- Naming: `{hazard_type}__{scenario_code}_h{return_period}glob`

**NetCDF (.nc) Files** - Modern scientific format with pre-computed statistics
- Auto-discovers from directory structure and file dimensions
- Direct extraction of pre-computed ensemble statistics (mean, median, p10, p90)
- Naming: `{hazard_type}__{indicator}__GWL={level}__RP={period}__ensemble={variant}`
- **No spatial computation needed** - statistics pre-computed in the NC file

#### Mixed Format Pipeline Handling

The pipeline seamlessly handles mixed TIF and NetCDF formats through several mechanisms:

**1. Unified Loading (`load_hazards_and_inventory()`)**
- Loads both formats in a single call
- Returns combined list: `list(hazards = list(tif = ..., nc = ...), inventory = ...)`
- Creates unified inventory with `source` column indicating data format
- TIF files are optional (if no mapping file exists, only NC files are loaded)

**2. Smart Event Filtering (`filter_hazards_by_events()`)**
- Filters hazards by event requirements across both formats
- **TIF hazards**: Exact name matching
- **NC hazards**: Automatic ensemble expansion (1 event → 4 ensemble variants)
- Returns single filtered list combining both formats

**3. Dual Extraction Workflow (`extract_hazard_statistics()`)**
- **Detects format mix** and chooses appropriate extraction method per hazard
- **TIF sources**: Spatial computation using `exactextractr` for pixel statistics
- **NC sources**: Direct extraction of pre-computed ensemble statistics
- **Priority cascade**: Coordinates → Municipality (ADM2) → Province (ADM1) → Error
- **Unified output**: Same column structure regardless of source format

**4. Combined Results Processing**
- All downstream functions work identically with mixed format results
- Damage factor joining, shock application, and financial calculations are format-agnostic
- Final results combine data from both formats seamlessly

#### Other Key Directories

- `tests/testthat/` - Unit and integration tests following TDD principles
- `tests/tests_data/` - Test data for development and testing
- `man/` - Auto-generated documentation (do not edit manually)
- `CONTEXT.md` - Development notes and function contracts

## Hazard Data Computation Methods

The climate risk tool offers two complementary approaches for computing hazard statistics:

### 1. Pre-computed Hazard Results (Applied for assets without lat/lon but regional information)

To generate the file of pre-computed results, use the pre-computation notebook:

**[`data-raw/notebooks/precompute_hazard_per_adm.ipynb`](data-raw/notebooks/precompute_hazard_per_adm.ipynb)**

This Python notebook:
- Processes hazard data (TIF and NetCDF) against administrative boundaries (ADM1/ADM2)
- Pre-computes hazard statistics for each region
- Generates `precomputed_adm_hazards.csv` with regional aggregates
- Supports both current climate and future scenarios (RCP2.6, RCP8.5)
- Handles ensemble statistics (mean, median, percentiles) for NetCDF data
- Significantly speeds up analysis by avoiding repeated spatial computations

### 2. Asset-to-Map Matching (Real-time Computation)

**Asset Size Consideration:**
- **Primary method**: Uses the actual asset size in square meters (m²) from asset data
- **Fallback method**: Uses a default area value when asset size is unavailable
- **Spatial computation**: Extracts hazard statistics using `exactextractr` for pixel-level accuracy
- **Format support**: Works with both GeoTIFF and NetCDF hazard data

**When to use each method:**
- **Pre-computed**: Large-scale analyses, repeated runs, performance-critical applications
- **Real-time matching**: Small datasets, maximum precision requirements, custom geometries

The tool seamlessly handles both approaches, allowing mixed usage in the same analysis pipeline.
