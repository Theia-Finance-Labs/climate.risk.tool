2
# climate.risk.tool

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/Theia-Finance-Labs/climate.risk.tool/graph/badge.svg)](https://app.codecov.io/gh/Theia-Finance-Labs/climate.risk.tool)
<!-- badges: end -->

The goal of climate.risk.tool is to ...

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
# - areas/municipality/ and areas/province/ with .geojson files
# - hazards/[hazard_type]: .tif raster files
# - damage_and_cost_factors.csv
base_dir <- "/path/to/your/data"

# Load all required data
assets <- read_assets(base_dir)
companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
hazards <- load_hazards(file.path(base_dir, "hazards"))
areas <- load_location_areas(
  file.path(base_dir, "areas", "municipality"),
  file.path(base_dir, "areas", "province")
)
damage_factors <- read_damage_cost_factors(base_dir)

# Create test events
events <- data.frame(
  event_id = "event_1",
  hazard_type = "flood",
  scenario = "rcp85",
  event_year = 2030,
  chronic = FALSE,
  stringsAsFactors = FALSE
)

# Run the complete climate risk analysis
results <- compute_risk(
  assets = assets,
  companies = companies,
  events = events,
  hazards = hazards,
  areas = areas,
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

- **`utils__*.R`** - Utility functions and data I/O
  - Input data reading, hazard inventory management
  - Area loading, result gathering and formatting

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

#### Other Key Directories

- `tests/testthat/` - Unit and integration tests following TDD principles
- `tests/tests_data/` - Test data for development and testing
- `man/` - Auto-generated documentation (do not edit manually)
- `WORKING_DOCUMENT.md` - Development notes and function contracts
