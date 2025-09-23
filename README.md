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

1. Run the analysis
Once the package is installed, you can:

- Call functions programmatically

``` r
library(climate.risk.tool)

# Path to your input data directory. It must contain:
# - user_input/: asset_information.csv, company.csv
# - areas/municipality/ and areas/province/ with .geojson files
# - hazards/: .tif raster files
# - damage_and_cost_factors.csv
base_dir <- "/path/to/your/data"

results <- compute_risk(
  base_dir = base_dir,
  shock_year = 2030,
  growth_rate = 0.02,
  net_profit_margin = 0.1,
  discount_rate = 0.05,
  verbose = TRUE
)

# Access results
results$assets
results$companies
``` 

- Launch the Shiny application

``` r
library(climate.risk.tool)
run_app()
```

This will open the climate.risk.tool interface, where you can upload data,
run the risk calculations, and view the results interactively


1. Developer 


Load all functions without reinstalling

``` r
devtools::load_all()
```

Run the development app with extra dev features (hot reloading, config options, etc.)

``` r
golem::run_dev()


Check documentation and tests

devtools::document()   # update function docs
devtools::test()       # run unit tests
devtools::check()      # full package checks
```