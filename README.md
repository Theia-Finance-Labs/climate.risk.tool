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

assets  <- data.frame(asset_id = c("A1", "A2"), country = c("FR", "DE"), value = c(10, 20))
hazards <- data.frame(country = c("FR", "DE"), hazard = c("flood", "flood"), intensity = c(0.5, 1.0))

res <- compute_risk(assets, hazards)
print(res)
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