#' Compute Climate Risk Analysis
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Executes the complete 16-step climate risk analysis pipeline from raw inputs
#'   to final risk metrics. This function serves as the main entry point and documentation
#'   center for the entire climate risk assessment workflow.
#'
#' @param base_dir Character string. Base directory containing input data subdirectories:
#'   - user_input/: asset_information.csv, company.csv
#'   - areas/: municipality/, province/ with .geojson files
#'   - hazards/: .tif raster files
#'   - damage_and_cost_factors.csv
#' @param shock_year Numeric. Year when acute climate shock occurs (for step 6)
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param net_profit_margin Numeric. Net profit margin assumption (default: 0.1)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' @param verbose Logical. Whether to print detailed progress messages (default: TRUE)
#'
#' @return List containing final results:
#'   - assets: Pivoted asset results with NPV by scenario
#'   - companies: Pivoted company results with NPV, PD, and Expected Loss by scenario
#'   - intermediate: List of intermediate results from each pipeline step
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
#' # Run complete climate risk analysis
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' results <- compute_risk(
#'   base_dir = base_dir,
#'   shock_year = 2030,
#'   growth_rate = 0.02,
#'   net_profit_margin = 0.1,
#'   discount_rate = 0.05
#' )
#' 
#' # Access final results
#' asset_results <- results$assets
#' company_results <- results$companies
#' 
#' # Access intermediate results for debugging
#' intermediate_data <- results$intermediate
#' }
#' @export
compute_risk <- function(base_dir, 
                        shock_year,
                        growth_rate = 0.02,
                        net_profit_margin = 0.1, 
                        discount_rate = 0.05,
                        verbose = TRUE) {
  
  if (verbose) {
    message("🚀 [compute_risk] Starting complete climate risk analysis pipeline")
    message("📂 Base directory: ", base_dir)
    message("⚡ Shock year: ", shock_year)
    message("📈 Growth rate: ", growth_rate)
    message("💰 Net profit margin: ", net_profit_margin)
    message("💸 Discount rate: ", discount_rate)
    message("")
  }
  
  # Initialize intermediate results storage
  intermediate <- list()
  
  # Step 1: Read inputs
  if (verbose) message("📋 Step 1/16: Reading input data...")
  inputs <- read_inputs(base_dir)
  assets <- inputs$assets
  companies <- inputs$companies
  intermediate$step01_inputs <- inputs
  
  # Step 2: Load hazards
  if (verbose) message("🗺️ Step 2/16: Loading hazard rasters...")
  hazards_dir <- file.path(base_dir, "hazards")
  hazards <- load_hazards(hazards_dir)
  intermediate$step02_hazards <- hazards
  
  # Step 3: Load location areas
  if (verbose) message("🌍 Step 3/16: Loading geographic boundaries...")
  municipalities_dir <- file.path(base_dir, "areas", "municipality")
  provinces_dir <- file.path(base_dir, "areas", "province")
  areas <- load_location_areas(municipalities_dir, provinces_dir)
  intermediate$step03_areas <- areas
  
  # Step 4: Geolocate assets
  if (verbose) message("📍 Step 4/16: Geolocating assets...")
  assets_with_geometry <- geolocate_assets(
    assets, hazards, areas$municipalities, areas$provinces
  )
  intermediate$step04_geolocated <- assets_with_geometry
  
  # Step 5: Cutout hazards
  if (verbose) message("✂️ Step 5/16: Extracting hazard values for assets...")
  assets_with_hazard_values <- cutout_hazards(assets_with_geometry, hazards)
  intermediate$step05_hazard_values <- assets_with_hazard_values
  
  # Step 6: Summarize hazards
  if (verbose) message("📊 Step 6/16: Summarizing hazard intensities...")
  assets_with_hazard_means <- summarize_hazards(assets_with_hazard_values)
  intermediate$step06_hazard_means <- assets_with_hazard_means
  
  # Step 7: Join damage and cost factors
  if (verbose) message("🔗 Step 7/16: Joining damage and cost factors...")
  factors_csv <- file.path(base_dir, "damage_and_cost_factors.csv")
  assets_with_factors <- join_damage_cost_factors(assets_with_hazard_means, factors_csv)
  intermediate$step07_with_factors <- assets_with_factors
  
  # Step 8: Apply acute shock
  if (verbose) message("⚡ Step 8/16: Applying acute climate shock...")
  assets_with_acute <- apply_acute_shock(assets_with_factors, shock_year)
  intermediate$step08_acute_shock <- assets_with_acute
  
  # Step 9: Apply chronic shock
  if (verbose) message("🌡️ Step 9/16: Applying chronic climate shock...")
  assets_with_chronic <- apply_chronic_shock(assets_with_acute)
  intermediate$step09_chronic_shock <- assets_with_chronic
  
  # Step 10: Compute asset impact
  if (verbose) message("💥 Step 10/16: Computing asset impact...")
  assets_with_impact <- compute_asset_impact(assets_with_chronic)
  intermediate$step10_asset_impact <- assets_with_impact
  
  # Step 11: Build scenarios
  if (verbose) message("🎯 Step 11/16: Building baseline vs shock scenarios...")
  # Create baseline version by resetting share_of_economic_activity to original values
  assets_baseline <- assets_with_impact
  # Match assets by asset_id and preserve order
  match_indices <- match(assets_baseline$asset_id, assets$asset_id)
  valid_matches <- !is.na(match_indices)
  if (any(valid_matches)) {
    assets_baseline$share_of_economic_activity[valid_matches] <- assets$share_of_economic_activity[match_indices[valid_matches]]
  }
  assets_scenarios <- build_scenarios(assets_baseline, assets_with_impact)
  intermediate$step11_scenarios <- assets_scenarios
  
  # Step 12: Compute asset revenue
  if (verbose) message("💵 Step 12/16: Computing asset revenue...")
  assets_with_revenue <- compute_asset_revenue(assets_scenarios, companies, growth_rate)
  intermediate$step12_revenue <- assets_with_revenue
  
  # Step 13: Compute asset profits
  if (verbose) message("💰 Step 13/16: Computing asset profits...")
  assets_with_profits <- compute_asset_profits(assets_with_revenue, net_profit_margin)
  intermediate$step13_profits <- assets_with_profits
  
  # Step 14: Discount net profits
  if (verbose) message("📉 Step 14/16: Discounting net profits...")
  assets_discounted <- discount_net_profits(assets_with_profits, discount_rate)
  intermediate$step14_discounted <- assets_discounted
  
  # Step 15: Compute company NPV
  if (verbose) message("🏢 Step 15/16: Computing company NPV...")
  companies_npv <- compute_company_npv(assets_discounted)
  intermediate$step15_npv <- companies_npv
  
  # Step 16: Compute company PD (Merton model)
  if (verbose) message("📊 Step 16/16: Computing probability of default...")
  companies_pd <- compute_company_pd_merton(companies_npv)
  intermediate$step16_pd <- companies_pd
  
  # Step 17: Compute expected loss
  if (verbose) message("⚠️ Step 17/16: Computing expected loss...")
  companies_el <- compute_expected_loss(companies_pd)
  intermediate$step17_expected_loss <- companies_el
  
  # Step 18: Gather and pivot results
  if (verbose) message("📋 Step 18/16: Gathering and pivoting final results...")
  final_results <- gather_and_pivot_results(assets_discounted, companies_el)
  intermediate$step18_final <- final_results
  
  if (verbose) {
    message("✅ [compute_risk] Pipeline completed successfully!")
    message("📊 Final results:")
    message("  - Assets: ", nrow(final_results$assets), " rows")
    message("  - Companies: ", nrow(final_results$companies), " rows")
    message("")
  }
  
  # Return comprehensive results
  list(
    assets = final_results$assets,
    companies = final_results$companies,
    intermediate = intermediate
  )
}
