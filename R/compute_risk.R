#' Compute Climate Risk Analysis (orchestrator)
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Orchestrates the climate risk assessment pipeline: (1) compute hazard event impacts on assets
#'   and (2) compute financial results from asset scenarios. This is the single entry point for analysis.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param companies Data frame containing company information (from read_companies())
#' @param events data.frame with columns `hazard_type`, `hazard_name`, `scenario_name`, `hazard_return_period`, `event_year` (or NA).
#'   The `event_id` column is auto-generated internally if not provided.
#' @param hazards Named list of SpatRaster objects (from load_hazards())
#' @param hazards_inventory Data frame with hazard metadata including hazard_indicator (from load_hazards_and_inventory()$inventory)
#' @param precomputed_hazards Data frame with precomputed hazard statistics for municipalities and states (from read_precomputed_hazards())
#' @param damage_factors Data frame with damage and cost factors (from read_damage_cost_factors())
#' @param cnae_exposure Optional tibble with CNAE exposure data for sector-based metric selection (from read_cnae_labor_productivity_exposure())
#' @param land_cover_legend Optional tibble with land cover legend for Fire hazard (from read_land_cover_legend())
#' @param adm1_boundaries Optional sf object with ADM1 (state) boundaries for state assignment and validation
#' @param adm2_boundaries Optional sf object with ADM2 (municipality) boundaries for state assignment via municipality lookup
#' @param validate_inputs Logical. If TRUE and boundaries are provided, validates input data coherence (default: TRUE)
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' @param risk_free_rate Numeric. Risk-free rate for Merton model (default: 0.02)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean").
#'   Valid options: "mean", "median", "p2_5", "p5", "p95", "p97_5", "max", "min", "p10", "p90".
#'   For TIF files: uses terra::extract with the specified function.
#'   For NC files: uses the mean ensemble layer by default.
#'   For precomputed data: uses the mean ensemble variant.
#' #'
#' @return List containing final results:
#'   - assets_factors: Asset-level hazard exposure with damage factors and event information (hazard_return_period, event_year)
#'   - companies: Pivoted company results with NPV, PD, and Expected Loss by scenario (aggregated)
#'   - assets_yearly: Detailed yearly asset trajectories with revenue, profit, and discounted values by year and scenario
#'   - companies_yearly: Detailed yearly company trajectories with aggregated revenue, profit, and discounted values by year and scenario
#'
#' @details
#' The function executes the following 16-step pipeline:
#' 1. Read inputs: Load asset and company data from CSV files
#' 2. Load hazards: Read climate hazard raster files (.tif)
#' 3. Load areas: Load municipality and state boundary files
#' 4. Geolocate assets: Add geometry and centroid columns using lat/lon > municipality > state priority
#' 5. Extract hazard statistics: Extract and aggregate hazard values for each asset geometry in long format
#' 6. Join damage factors: Map hazard intensity to damage/cost factors
#' 7. Apply acute shock: Calculate sudden climate event impacts
#' 8. Compute asset impact: Update share_of_economic_activity with all impacts
#' 9. Build scenarios: Create baseline vs shock scenario data
#' 10. Compute asset revenue: Allocate company revenue to assets
#' 11. Compute asset profits: Apply company-specific net profit margins from company file
#' 12. Discount net profits: Apply present value discounting
#' 13. Compute company NPV: Aggregate asset profits to company level
#' 14. Compute company PD: Calculate probability of default using Merton model
#' 15. Compute expected loss: Calculate expected loss using EL = LGD * Loan_Size * PD
#' 16. Gather and pivot results: Transform to wide format for reporting
#'
#' @examples
#' \dontrun{
#' # Load required data
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' precomputed_hazards <- read_precomputed_hazards(base_dir)
#' damage_factors <- read_damage_cost_factors(base_dir)
#' cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
#'
#' # Define events
#' events <- data.frame(
#'   hazard_type = "flood",
#'   scenario = "rcp85",
#'   event_year = 2030
#' )
#'
#' # Run analysis
#' results <- compute_risk(
#'   assets = assets,
#'   companies = companies,
#'   events = events,
#'   hazards = hazards,
#'   hazards_inventory = hazards_inventory,
#'   precomputed_hazards = precomputed_hazards,
#'   damage_factors = damage_factors,
#'   cnae_exposure = cnae_exposure,
#'   growth_rate = 0.02,
#'   discount_rate = 0.05,
#'   risk_free_rate = 0.02
#' )
#'
#' # Access final results
#' asset_results <- results$assets # Aggregated asset NPV by scenario
#' company_results <- results$companies # Aggregated company NPV, PD, EL by scenario
#' asset_yearly <- results$assets_yearly # Detailed yearly asset trajectories
#' company_yearly <- results$companies_yearly # Detailed yearly company trajectories
#' }
#' @export
compute_risk <- function(assets,
                         companies,
                         events,
                         hazards,
                         hazards_inventory,
                         precomputed_hazards,
                         damage_factors,
                         cnae_exposure = NULL,
                         land_cover_legend = NULL,
                         adm1_boundaries = NULL,
                         adm2_boundaries = NULL,
                         validate_inputs = TRUE,
                         growth_rate = 0.02,
                         discount_rate = 0.05,
                         risk_free_rate = 0.02,
                         aggregation_method = "mean") {
  # Validate inputs
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("assets must be a non-empty data.frame (from read_assets())")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("companies must be a non-empty data.frame (from read_companies())")
  }
  if (!is.data.frame(events) || nrow(events) == 0) {
    stop("events must be a non-empty data.frame with hazard_type, hazard_name, event_year")
  }
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list of SpatRaster objects (from load_hazards())")
  }
  if (!is.data.frame(precomputed_hazards) || nrow(precomputed_hazards) == 0) {
    stop("precomputed_hazards must be a non-empty data.frame (from read_precomputed_hazards())")
  }
  if (!is.data.frame(damage_factors) || nrow(damage_factors) == 0) {
    stop("damage_factors must be a non-empty data.frame (from read_damage_cost_factors())")
  }

  # Validate aggregation_method
  valid_aggregation_methods <- c("mean", "median", "p2_5", "p5", "p95", "p97_5", "max", "min", "p10", "p90")
  if (!aggregation_method %in% valid_aggregation_methods) {
    stop("aggregation_method must be one of: ", paste(valid_aggregation_methods, collapse = ", "))
  }

  # ============================================================================
  # PHASE 0: INPUT PREPARATION - Assign states to assets and validate
  # ============================================================================

  # Assign states to assets that don't have one (requires boundaries)
  if (!is.null(adm1_boundaries)) {
    message("[compute_risk] Assigning states to assets without location data...")
    assets <- assign_state_to_assets_with_boundaries(
      assets,
      adm1_boundaries,
      adm2_boundaries
    )
  }

  # Validate input data coherence
  if (validate_inputs && !is.null(adm1_boundaries)) {
    message("[compute_risk] Validating input data coherence...")

    # Extract boundary names for validation
    adm1_names <- adm1_boundaries |>
      dplyr::pull(.data$shapeName) |>
      as.character() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      unique()

    adm2_names <- if (!is.null(adm2_boundaries)) {
      adm2_boundaries |>
        dplyr::pull(.data$shapeName) |>
        as.character() |>
        stringi::stri_trans_general("Latin-ASCII") |>
        unique()
    } else {
      character(0)
    }

    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      damage_factors_df = damage_factors,
      precomputed_hazards_df = precomputed_hazards,
      cnae_exposure_df = cnae_exposure,
      adm1_names = adm1_names,
      adm2_names = adm2_names,
      events_df = events
    )
  }

  # ============================================================================
  # PHASE 1: UTILS - Data preparation
  # ============================================================================

  # Auto-generate event_id if not provided (only if column doesn't exist)
  if (!"event_id" %in% names(events)) {
    events <- events |>
      dplyr::mutate(event_id = paste0("event_", dplyr::row_number()))
  }

  duplicated_event_ids <- events$event_id[duplicated(events$event_id)]
  if (length(duplicated_event_ids) > 0) {
    stop(
      "events must have unique event_id values; duplicates found: ",
      paste(unique(duplicated_event_ids), collapse = ", ")
    )
  }

  # Filter assets to only include those with matching companies
  assets <- filter_assets_by_companies(assets, companies)

  # Filter hazards to only those referenced by events
  # Note: For multi-indicator hazards (Fire), this will internally expand to load all required indicators
  # Note: For NC hazards, only the mean ensemble is loaded by default
  hazards <- filter_hazards_by_events(hazards, events, hazards_inventory)


  # ============================================================================
  # PHASE 2: GEOSPATIAL - Extract hazard statistics (spatial or precomputed)
  # ============================================================================

  # Filter inventory to match filtered hazards (prevent warnings about unfound hazards)
  filtered_hazard_names <- names(hazards)
  filtered_inventory <- hazards_inventory |>
    dplyr::filter(.data$hazard_name %in% filtered_hazard_names)

  # Extract hazard statistics: spatial extraction for assets with coordinates,
  # precomputed lookup for assets with municipality/state only

  assets_long <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards,
    hazards_inventory = filtered_inventory,
    precomputed_hazards = precomputed_hazards,
    aggregation_method = aggregation_method,
    damage_factors_df = damage_factors
  )

  # Step 2.3: Join event information (event_year, scenario_name) from events
  # For multi-indicator hazards (Fire), create a mapping from all indicator hazard_names to the event
  # For single-indicator hazards, use hazard_name directly
  events_expanded_for_join <- create_event_hazard_mapping(events, hazards_inventory, aggregation_method)

  assets_with_events <- assets_long |>
    dplyr::inner_join(
      events_expanded_for_join |> dplyr::select("hazard_name", "event_id", "event_year"),
      by = "hazard_name", relationship = "many-to-many"
    )

  # Step 2.4: Join damage cost factors (needs scenario_name for Heat hazards, land_cover_legend for Fire)
  assets_factors <- join_damage_cost_factors(assets_with_events, damage_factors, cnae_exposure, land_cover_legend)


  # ============================================================================
  # PHASE 3: SHOCK - Compute baseline and shocked yearly trajectories
  # ============================================================================

  # Step 3.1: Compute baseline yearly trajectories
  yearly_baseline <- compute_baseline_trajectories(
    baseline_assets = assets,
    companies = companies,
    growth_rate = growth_rate
  )

  # Step 3.2: Compute shocked trajectories and concatenate with baseline
  # This now returns both baseline and shock scenarios in one dataframe
  yearly_shock <- compute_shock_trajectories(
    yearly_baseline_profits = yearly_baseline,
    assets_with_factors = assets_factors,
    events = events,
    companies = companies
  )

  yearly_scenarios <- concatenate_baseline_and_shock(yearly_baseline, yearly_shock)

  # ============================================================================
  # PHASE 4: FINANCIAL_ASSETS - Asset-level financial computations
  # ============================================================================

  # Apply discounting to yearly scenarios
  assets_discounted_yearly <- discount_yearly_profits(yearly_scenarios, discount_rate)


  # ============================================================================
  # PHASE 5: FINANCIALS_COMPANY - Company-level aggregation and risk metrics
  # ============================================================================

  # Compute company-level yearly trajectories for detailed analysis
  company_yearly_trajectories <- aggregate_assets_to_company(assets_discounted_yearly)

  # Use companies financials function that works with yearly data
  companies_result <- compute_companies_financials(companies, company_yearly_trajectories, assets_discounted_yearly, discount_rate, risk_free_rate)


  # ============================================================================
  # PHASE 6: UTILS - Final result formatting and output
  # ============================================================================

  # Final results include both aggregated and yearly trajectory data
  final_results <- list(
    assets_factors = assets_factors,
    companies = companies_result,
    assets_yearly = assets_discounted_yearly,
    companies_yearly = company_yearly_trajectories
  )


  final_results
}
