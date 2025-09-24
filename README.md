2
# climate.risk.tool

<!-- badges: start -->
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
# - hazards/: .tif raster files
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
damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")

# Run the complete climate risk analysis
results <- compute_risk(
  assets = assets,
  companies = companies,
  hazards = hazards,
  areas = areas,
  damage_factors = damage_factors_path,
  shock_year = 2030,
  growth_rate = 0.02,
  net_profit_margin = 0.1,
  discount_rate = 0.05,
  verbose = TRUE
)

# Access results
results$assets
results$companies
results$intermediate  # Access intermediate pipeline results
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

### 5. Package Structure

- `R/` - Main package functions
- `tests/testthat/` - Unit and integration tests  
- `tests/tests_data/` - Test data for development and testing
- `WORKING_DOCUMENT.md` - Development notes and function contracts