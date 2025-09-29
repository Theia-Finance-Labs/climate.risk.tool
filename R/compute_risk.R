#' Compute Climate Risk Analysis (orchestrator)
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Orchestrates the split pipeline: (1) compute hazard event impacts on assets
#'   and (2) compute financial results from asset scenarios. This replaces the previous
#'   monolithic pipeline and is now the single entry point.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param companies Data frame containing company information (from read_companies())
#' @param events data.frame with columns `hazard_type`, `scenario`, `event_year` (or NA), `chronic`.
#'   When multiple rows are provided, events are combined (currently min share per asset).
#' @param precomputed_assets_factors Either a data.frame with precomputed assets factors, 
#'   or a character string path to a precomputed RDS file (REQUIRED)
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
#' # Method 1: Using precomputed assets factors (RECOMMENDED - much faster)
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
#' 
#' # Precompute assets factors once (this is the slow step)
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' areas <- load_location_areas(
#'   file.path(base_dir, "areas", "municipality"),
#'   file.path(base_dir, "areas", "province")
#' )
#' damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
#' 
#' precomputed_file <- precompute_assets_factors(
#'   assets = assets,
#'   hazards = hazards,
#'   areas = areas,
#'   damage_factors = damage_factors_path,
#'   hazards_dir = file.path(base_dir, "hazards")
#' )
#'
#' # Now run analysis with precomputed data (fast!)
#' events <- data.frame(
#'   hazard_type = "flood",
#'   scenario = "rcp85",
#'   event_year = 2030,
#'   chronic = FALSE
#' )
#'
#' results <- compute_risk(
#'   assets = assets,
#'   companies = companies,
#'   events = events,
#'   precomputed_assets_factors = precomputed_file,
#'   growth_rate = 0.02,
#'   net_profit_margin = 0.1,
#'   discount_rate = 0.05
#' )
#' 
#' 
#' # Access final results
#' asset_results <- results$assets  # Aggregated asset NPV by scenario
#' company_results <- results$companies  # Aggregated company NPV, PD, EL by scenario
#' asset_yearly <- results$assets_yearly  # Detailed yearly asset trajectories
#' company_yearly <- results$companies_yearly  # Detailed yearly company trajectories
#' 

#' }
#' @export
compute_risk <- function(assets,
                        companies,
                        events,
                        precomputed_assets_factors,
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
    stop("events must be a non-empty data.frame with hazard_type, scenario, event_year/chronic")
  }
  if (is.null(precomputed_assets_factors)) {
    stop("precomputed_assets_factors is required (use precompute_assets_factors() to create)")
  }
  
  
  # ============================================================================
  # PHASE 1: UTILS - Input validation and data preparation
  # ============================================================================
  
  # Prepare baseline asset data for later use
  baseline_assets <- assets[, c("asset", "share_of_economic_activity", "company"), drop = FALSE]
  
  
  # ============================================================================
  # PHASE 2: GEOSPATIAL - Asset geolocation and hazard processing
  # ============================================================================
  
  # Get assets with geospatial hazard data and damage cost factors
  if (is.character(precomputed_assets_factors)) {
    # Load from file
    assets_factors <- load_precomputed_assets_factors(precomputed_assets_factors)
  } else if (is.data.frame(precomputed_assets_factors)) {
    # Use provided data frame
    assets_factors <- precomputed_assets_factors
  } else {
    stop("precomputed_assets_factors must be a data.frame or file path")
  }
  
  
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
