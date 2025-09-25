#' Compute Climate Risk Analysis (orchestrator)
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Orchestrates the split pipeline: (1) compute hazard event impacts on assets
#'   and (2) compute financial results from asset scenarios. This replaces the previous
#'   monolithic pipeline and is now the single entry point.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param companies Data frame containing company information (from read_companies())
#' @param hazards Named list of SpatRaster objects (from load_hazards())
#' @param areas List containing municipalities and provinces named lists (from load_location_areas())
#' @param damage_factors Data frame with damage and cost factors, or path to CSV file
#' @param events data.frame with columns `hazard_type`, `scenario`, `event_year` (or NA), `chronic`.
#'   When multiple rows are provided, events are combined (currently min share per asset).
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param net_profit_margin Numeric. Net profit margin assumption (default: 0.1)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' @param verbose Logical. Whether to print detailed progress messages (default: TRUE)
#'
#' @return List containing final results:
#'   - assets: Pivoted asset results with NPV by scenario
#'   - companies: Pivoted company results with NPV, PD, and Expected Loss by scenario
#'
#' @details
#' The function executes the following 16-step pipeline:
#' 1. Read inputs: Load asset and company data from CSV files
#' 2. Load hazards: Read climate hazard raster files (.tif)
#' 3. Load areas: Load municipality and province boundary files
#' 4. Geolocate assets: Add geometry and centroid columns using lat/lon > municipality > province priority
#' 5. Cutout hazards: Extract hazard values for each asset geometry
#' 6. Summarize hazards: Calculate mean hazard intensity per asset
#' 7. Join damage factors: Map hazard intensity to damage/cost factors
#' 8. Apply acute shock: Calculate sudden climate event impacts
#' 9. Apply chronic shock: Calculate gradual climate change impacts
#' 10. Compute asset impact: Update share_of_economic_activity with all impacts
#' 11. Build scenarios: Create baseline vs shock scenario data
#' 12. Compute asset revenue: Allocate company revenue to assets
#' 13. Compute asset profits: Apply net profit margins
#' 14. Discount net profits: Apply present value discounting
#' 15. Compute company NPV: Aggregate asset profits to company level
#' 16. Compute company PD: Calculate probability of default using Merton model
#' 17. Compute expected loss: Calculate expected loss using EL = LGD * Loan_Size * PD
#' 18. Gather and pivot results: Transform to wide format for reporting
#'
#' @examples
#' \dontrun{
#' # Load data first
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' areas <- load_location_areas(
#'   file.path(base_dir, "areas", "municipality"),
#'   file.path(base_dir, "areas", "province")
#' )
#' damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
#' events <- data.frame(
#'   hazard_type = "flood",
#'   scenario = "rcp85",
#'   event_year = 2030,
#'   chronic = FALSE
#' )
#'
#' # Run complete climate risk analysis
#' results <- compute_risk(
#'   assets = assets,
#'   companies = companies,
#'   hazards = hazards,
#'   areas = areas,
#'   damage_factors = damage_factors_path,
#'   events = events,
#'   growth_rate = 0.02,
#'   net_profit_margin = 0.1,
#'   discount_rate = 0.05
#' )
#' 
#' # Access final results
#' asset_results <- results$assets
#' company_results <- results$companies
#' 

#' }
#' @export
compute_risk <- function(assets,
                        companies,
                        hazards,
                        areas,
                        damage_factors,
                        events,
                        growth_rate = 0.02,
                        net_profit_margin = 0.1,
                        discount_rate = 0.05,
                        verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("assets must be a non-empty data.frame (from read_assets())")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("companies must be a non-empty data.frame (from read_companies())")
  }
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list of SpatRaster objects (from load_hazards())")
  }
  if (!is.list(areas) || !all(c("municipalities", "provinces") %in% names(areas))) {
    stop("areas must be a list with 'municipalities' and 'provinces' elements (from load_location_areas())")
  }
  if (!is.data.frame(events) || nrow(events) == 0) {
    stop("events must be a non-empty data.frame with hazard_type, scenario, event_year/chronic")
  }
  
  if (verbose) {
    message("🚀 [compute_risk] Starting complete climate risk analysis pipeline")
    message("📊 Input data: ", nrow(assets), " assets, ", nrow(companies), " companies")
    message("🗺️ Hazards: ", length(hazards), " hazard layers loaded")
    message("🌍 Areas: ", length(areas$municipalities), " municipalities, ", length(areas$provinces), " provinces")
  }
  events_assets <- compute_hazard_events(assets, hazards, areas, events, damage_factors, verbose = verbose)

  # ---- TODO what is this
  baseline_assets <- assets[, c("asset", "share_of_economic_activity"), drop = FALSE]
  names(baseline_assets)[names(baseline_assets) == "share_of_economic_activity"] <- "baseline_share"
  ea_min <- events_assets[, c("asset", "company", "share_of_economic_activity"), drop = FALSE]
  shocked <- merge(ea_min, baseline_assets, by = "asset")
  shocked$share_of_economic_activity <- pmin(shocked$share_of_economic_activity, shocked$baseline_share)
  shocked_assets <- stats::aggregate(share_of_economic_activity ~ asset + company, data = shocked[, c("asset", "company", "share_of_economic_activity")], FUN = min)
  baseline_assets <- assets[, c("asset", "share_of_economic_activity", "company"), drop = FALSE]
  # ----------------

  assets_scenarios <- build_scenarios(baseline_assets, shocked_assets)
  fin <- compute_financials_from_assets(assets_scenarios, companies, growth_rate, net_profit_margin, discount_rate)
  
  list(assets = fin$assets, companies = fin$companies)
}
