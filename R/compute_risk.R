#' Compute Climate Risk Analysis (orchestrator)
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Orchestrates the climate risk assessment pipeline: (1) compute hazard event impacts on assets
#'   and (2) compute financial results from asset scenarios. This is the single entry point for analysis.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param companies Data frame containing company information (from read_companies())
#' @param events data.frame with columns `hazard_type`, `scenario`, `event_year` (or NA), `chronic`.
#'   When multiple rows are provided, events are combined (currently min share per asset).
#' @param hazards Named list of SpatRaster objects (from load_hazards())
#' @param areas List containing municipalities and provinces named lists (from load_location_areas())
#' @param damage_factors Data frame with damage and cost factors (from read_damage_cost_factors())
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param net_profit_margin Numeric. Net profit margin assumption (default: 0.1)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' #'
#' @return List containing final results:
#'   - assets: Pivoted asset results with NPV by scenario (aggregated)
#'   - companies: Pivoted company results with NPV, PD, and Expected Loss by scenario (aggregated)
#'   - assets_yearly: Detailed yearly asset trajectories with revenue, profit, and discounted values by year and scenario
#'   - companies_yearly: Detailed yearly company trajectories with aggregated revenue, profit, and discounted values by year and scenario
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
#' # Load required data
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' areas <- load_location_areas(
#'   file.path(base_dir, "areas", "municipality"),
#'   file.path(base_dir, "areas", "province")
#' )
#' damage_factors <- read_damage_cost_factors(base_dir)
#' 
#' # Define events
#' events <- data.frame(
#'   hazard_type = "flood",
#'   scenario = "rcp85",
#'   event_year = 2030,
#'   chronic = FALSE
#' )
#'
#' # Run analysis
#' results <- compute_risk(
#'   assets = assets,
#'   companies = companies,
#'   events = events,
#'   hazards = hazards,
#'   areas = areas,
#'   damage_factors = damage_factors,
#'   growth_rate = 0.02,
#'   net_profit_margin = 0.1,
#'   discount_rate = 0.05
#' )
#' 
#' # Access final results
#' asset_results <- results$assets  # Aggregated asset NPV by scenario
#' company_results <- results$companies  # Aggregated company NPV, PD, EL by scenario
#' asset_yearly <- results$assets_yearly  # Detailed yearly asset trajectories
#' company_yearly <- results$companies_yearly  # Detailed yearly company trajectories
#' }
#' @export
compute_risk <- function(assets,
                        companies,
                        events,
                        hazards,
                        areas,
                        damage_factors,
                        growth_rate = 0.02,
                        net_profit_margin = 0.1,
                        discount_rate = 0.05
) {
  
  # Validate inputs
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("assets must be a non-empty data.frame (from read_assets())")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("companies must be a non-empty data.frame (from read_companies())")
  }
  if (!is.data.frame(events) || nrow(events) == 0) {
    stop("events must be a non-empty data.frame with hazard_type, hazard_name, event_year/chronic")
  }
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list of SpatRaster objects (from load_hazards())")
  }
  if (!is.list(areas) || !all(c("municipalities", "provinces") %in% names(areas))) {
    stop("areas must be a list with 'municipalities' and 'provinces' elements (from load_location_areas())")
  }
  if (!is.data.frame(damage_factors) || nrow(damage_factors) == 0) {
    stop("damage_factors must be a non-empty data.frame (from read_damage_cost_factors())")
  }
  
  
  # ============================================================================
  # PHASE 1: UTILS - Input validation and data preparation
  # ============================================================================
  
  # Prepare baseline asset data for later use
  baseline_assets <- assets[, c("asset", "share_of_economic_activity", "company"), drop = FALSE]
  
  
  # ============================================================================
  # PHASE 2: GEOSPATIAL - Asset geolocation and hazard processing
  # ============================================================================
  
  # Step 2.1: Geolocate assets
  assets_geo <- geolocate_assets(assets, hazards, areas$municipalities, areas$provinces)
  
  # Step 2.2: Cutout hazards
  assets_cut <- cutout_hazards(assets_geo, hazards)
  
  # Step 2.3: Summarize hazards
  assets_long <- summarize_hazards(assets_cut)
  
  # Step 2.4: Join damage cost factors
  assets_factors <- join_damage_cost_factors(assets_long, damage_factors)
  
  
  # ============================================================================
  # PHASE 3: SHOCK - Compute baseline and shocked yearly trajectories
  # ============================================================================
  
  # Step 3.1: Compute baseline yearly trajectories
  yearly_baseline_profits <- compute_baseline_trajectories(
    baseline_assets = baseline_assets,
    companies = companies,
    growth_rate = growth_rate,
    net_profit_margin = net_profit_margin
  )
  
  # Step 3.2: Compute shocked yearly trajectories
  yearly_shocked_profits <- compute_shock_trajectories(
    yearly_baseline_profits = yearly_baseline_profits,
    assets_with_factors = assets_factors,
    events = events
  )
  
  # Step 3.3: Build scenarios (concatenate baseline and shocked)
  yearly_scenarios <- build_yearly_scenarios(yearly_baseline_profits, yearly_shocked_profits)
  
  
  # ============================================================================
  # PHASE 4: FINANCIAL_ASSETS - Asset-level financial computations
  # ============================================================================
  
  # Apply discounting to yearly scenarios
  assets_discounted_yearly <- discount_yearly_profits(yearly_scenarios, discount_rate)
  
  
  # ============================================================================
  # PHASE 5: FINANCIALS_COMPANY - Company-level aggregation and risk metrics
  # ============================================================================
  
  # Compute company-level yearly trajectories for detailed analysis
  company_yearly_trajectories <- compute_company_yearly_trajectories(assets_discounted_yearly)
  
  # Use companies financials function that works with yearly data
  fin <- compute_companies_financials(company_yearly_trajectories, assets_discounted_yearly, discount_rate)
  
  
  # ============================================================================
  # PHASE 6: UTILS - Final result formatting and output
  # ============================================================================
  
  # Final results include both aggregated and yearly trajectory data
  final_results <- list(
    assets_factors=assets_factors,
    assets = fin$assets, 
    companies = fin$companies,
    assets_yearly = assets_discounted_yearly,
    companies_yearly = company_yearly_trajectories
  )
  
  
  final_results
}
