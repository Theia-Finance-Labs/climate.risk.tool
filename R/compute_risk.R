#' Compute Climate Risk Analysis (orchestrator)
#'
#' @title Main orchestrator for climate risk assessment pipeline
#' @description Orchestrates the climate risk assessment pipeline: (1) compute hazard event impacts on assets
#'   and (2) compute financial results from asset scenarios. This is the single entry point for analysis.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param companies Data frame containing company information (from read_companies())
#' @param events data.frame with columns `hazard_type`, `hazard_name`, `scenario_name`, `return_period`, `event_year` (or NA).
#'   The `event_id` column is auto-generated internally if not provided.
#' @param hazards Named list of SpatRaster objects (from load_hazards())
#' @param hazards_inventory Data frame with hazard metadata including hazard_indicator (from load_hazards_and_inventory()$inventory)
#' @param precomputed_hazards Data frame with precomputed hazard statistics for municipalities and states (from read_precomputed_hazards())
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#' @param hazards_dir Character path to hazards/config directory containing hazard YAML files
#' @param cnae_exposure Data frame with CNAE exposure mapping (must include `cnae` and `description`)
#' @param adm1_boundaries Optional sf object with ADM1 (state) boundaries for state assignment and validation
#' @param adm2_boundaries Optional sf object with ADM2 (municipality) boundaries for state assignment via municipality lookup
#' @param validate_inputs Logical. If TRUE and boundaries are provided, validates input data coherence (default: TRUE)
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' @param risk_free_rate Numeric. Risk-free rate for Merton model (default: 0.02)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean").
#'   Valid options: "mean", "median", "p90", "p10", "max", "min", "mode", "closest".
#'   For NetCDF files: uses terra::extract with the specified function.
#'   For NC files: uses the mean ensemble layer by default (ensemble selection is separate from aggregation_method).
#'   For precomputed data: uses the mean ensemble variant (ensemble selection is separate from aggregation_method).
#' #'
#' @return List containing final results:
#'   - assets_factors: Asset-level hazard exposure with damage factors and event information (return_period, event_year)
#'   - companies: Pivoted company results with NPV, PD, and Expected Loss by scenario (aggregated)
#'   - assets_yearly: Detailed yearly asset trajectories with revenue, profit, and discounted values by year and scenario
#'   - companies_yearly: Detailed yearly company trajectories with aggregated revenue, profit, and discounted values by year and scenario
#'
#' @details
#' The function executes the following 16-step pipeline:
#' 1. Read inputs: Load asset and company data from CSV files
#' 2. Load hazards: Read climate hazard NetCDF files (.nc)
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
#' input_folder <- "/path/to/folder/with/excel/files"
#' assets <- read_assets(input_folder)
#' companies <- read_companies(input_folder)
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' precomputed_hazards <- read_precomputed_hazards(base_dir)
#' hazard_configs <- hazard_data$configs
#' cnae_exposure <- load_mapping_from_config(base_dir, hazard_configs, "Heat", "cnae_exposure")
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
#'   hazard_configs = hazard_configs,
#'   hazards_dir = file.path(base_dir, "hazards"),
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
                         hazard_configs,
                         hazards_dir,
                         cnae_exposure,
                         adm1_boundaries = NULL,
                         adm2_boundaries = NULL,
                         spatial_separation_data = NULL,
                         base_dir = NULL,
                         validate_inputs = TRUE,
                         growth_rate = 0.02,
                         discount_rate = 0.05,
                         risk_free_rate = 0.02,
                         aggregation_method = "mean",
                         on_progress = NULL) {
  # Internal helper: always print an ASCII bar to the R console AND call
  # the optional on_progress callback (e.g. for a Shiny progress widget).
  .report_progress <- function(value, msg) {
    bar_width <- 30
    filled <- round(value * bar_width)
    bar <- paste0(
      "[", paste(rep("=", filled), collapse = ""),
      if (filled < bar_width) ">" else "",
      paste(rep(" ", max(0, bar_width - filled - 1)), collapse = ""),
      "]"
    )
    message(sprintf("%s %3d%%  %s", bar, round(value * 100), msg))
    if (!is.null(on_progress)) {
      tryCatch(
        on_progress(value, msg),
        error = function(e) message("[progress callback error] ", conditionMessage(e))
      )
    }
  }

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
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs must be a non-empty list from load_hazards_and_inventory()")
  }
  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    stop("hazards_dir must be a valid directory path")
  }
  if (!is.data.frame(cnae_exposure) || nrow(cnae_exposure) == 0) {
    stop("cnae_exposure must be a non-empty data.frame with CNAE sector mapping")
  }

  # Validate aggregation_method
  valid_aggregation_methods <- c("mean", "median", "p90", "p10", "max", "min", "mode", "closest")
  if (!aggregation_method %in% valid_aggregation_methods) {
    stop("aggregation_method must be one of: ", paste(valid_aggregation_methods, collapse = ", "))
  }

  # ============================================================================
  # PHASE 0: INPUT PREPARATION - Assign states to assets and validate
  # ============================================================================
  .report_progress(0.05, "Preparing inputs...")

  # Assign ADM regions (state/municipality) from boundaries when available
  if (!is.null(adm1_boundaries)) {
    message("[compute_risk] Assigning ADM regions to assets...")
    adm_codes <- NULL
    if (!is.null(base_dir) && nzchar(base_dir)) {
      adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")
      if (file.exists(adm_codes_path)) {
        adm_codes <- load_adm_codes_from_path(adm_codes_path)
      }
    }
    adm1_boundaries <- repair_adm_boundary_names(adm1_boundaries, adm_codes, "adm1")
    adm2_boundaries <- repair_adm_boundary_names(adm2_boundaries, adm_codes, "adm2")
    assets <- assign_state_to_assets_with_boundaries(
      assets,
      adm1_boundaries,
      adm2_boundaries,
      adm_codes = adm_codes
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

    adm1_shape_ids <- if ("shapeID" %in% names(adm1_boundaries)) {
      unique(as.character(adm1_boundaries$shapeID))
    } else {
      character(0)
    }
    adm2_shape_ids <- if (!is.null(adm2_boundaries) && "shapeID" %in% names(adm2_boundaries)) {
      unique(as.character(adm2_boundaries$shapeID))
    } else {
      character(0)
    }
    adm1_codes <- if (!is.null(adm_codes) && all(c("code", "adm_level") %in% names(adm_codes))) {
      adm_codes |>
        dplyr::filter(.data$adm_level == "adm1") |>
        dplyr::pull(.data$code) |>
        as.character() |>
        unique()
    } else {
      character(0)
    }
    adm2_codes <- if (!is.null(adm_codes) && all(c("code", "adm_level") %in% names(adm_codes))) {
      adm_codes |>
        dplyr::filter(.data$adm_level == "adm2") |>
        dplyr::pull(.data$code) |>
        as.character() |>
        unique()
    } else {
      character(0)
    }

    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = hazards_dir,
      hazard_configs = hazard_configs,
      cnae_exposure_df = cnae_exposure,
      precomputed_hazards_df = precomputed_hazards,
      adm1_names = adm1_names,
      adm2_names = adm2_names,
      adm1_codes = adm1_codes,
      adm2_codes = adm2_codes,
      adm1_shape_ids = adm1_shape_ids,
      adm2_shape_ids = adm2_shape_ids,
      events_df = events
    )
  }

  # ============================================================================
  # PHASE 1: UTILS - Data preparation
  # ============================================================================
  .report_progress(0.15, "Preparing data...")

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
  hazards <- filter_hazards_by_events(hazards, events, hazards_inventory, hazard_configs)


  # ============================================================================
  # PHASE 2: GEOSPATIAL - Extract hazard statistics (spatial or precomputed)
  # ============================================================================
  .report_progress(0.25, "Extracting hazard statistics...")

  # Filter inventory to match filtered hazards (prevent warnings about unfound hazards)
  filtered_hazard_keys <- names(hazards)
  filtered_inventory <- hazards_inventory |>
    dplyr::filter(.data$indicator_key %in% filtered_hazard_keys)

  # Extract hazard statistics: spatial extraction for assets with coordinates,
  # precomputed lookup for assets with municipality/state only
  assets_long <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards,
    hazards_inventory = filtered_inventory,
    precomputed_hazards = precomputed_hazards,
    hazard_configs = hazard_configs,
    aggregation_method = aggregation_method
  )

  # Step 2.3: Join event information (event_year, scenario_name) from events
  # For multi-indicator hazards (Fire), create a mapping from all indicator hazard_names to the event
  # For single-indicator hazards, use hazard_name directly
  events_expanded_for_join <- create_event_hazard_mapping(events, hazards_inventory, hazard_configs)
  
    # Join with explicit suffixes to handle overlapping columns between extraction and events
    # We prioritize event-level metadata (like scenario_name, return_period) over
    # metadata extracted from raster filenames.
    assets_with_events <- assets_long |>
      dplyr::inner_join(
        events_expanded_for_join,
        by = "indicator_key",
        suffix = c(".extracted", ".event"),
        relationship = "many-to-many"
      )
    
    # Identify all columns that came from the event table (they have .event suffix or no suffix if unique)
    event_cols <- names(events_expanded_for_join)
    # Preserve per-indicator metadata from extraction
    event_cols <- setdiff(event_cols, c("indicator_key", "hazard_indicator", "hazard_type", "matching_method"))
    
    # CRITICAL: Filter out columns that don't exist in assets_with_events
    # This can happen if events_expanded_for_join has columns not present in assets_long
    event_cols <- intersect(event_cols, names(assets_with_events))
    
    for (col in event_cols) {
      event_col_name <- if (paste0(col, ".event") %in% names(assets_with_events)) paste0(col, ".event") else col
      extracted_col_name <- paste0(col, ".extracted")
      
      # If we have an event-specific version of this column, use it to overwrite/create the main column
      if (event_col_name %in% names(assets_with_events)) {
        # CRITICAL FIX: Use simple assignment instead of mutate to avoid many-to-many issues
        # with character columns that might contain commas or other special characters
        assets_with_events[[col]] <- assets_with_events[[event_col_name]]
        
        # Clean up the suffixed columns
        if (event_col_name != col) {
          assets_with_events[[event_col_name]] <- NULL
        }
        if (extracted_col_name %in% names(assets_with_events)) {
          assets_with_events[[extracted_col_name]] <- NULL
        }
      }
    }

    # Ensure matching_method is preserved from extraction
    if (paste0("matching_method", ".extracted") %in% names(assets_with_events)) {
      assets_with_events$matching_method <- assets_with_events[["matching_method.extracted"]]
      assets_with_events[["matching_method.extracted"]] <- NULL
    }
    if (paste0("matching_method", ".event") %in% names(assets_with_events)) {
      assets_with_events[["matching_method.event"]] <- NULL
    }

    # Ensure hazard_type is preserved (required downstream)
    if (paste0("hazard_type", ".extracted") %in% names(assets_with_events)) {
      assets_with_events$hazard_type <- assets_with_events[["hazard_type.extracted"]]
      assets_with_events[["hazard_type.extracted"]] <- NULL
    }
    if (paste0("hazard_type", ".event") %in% names(assets_with_events)) {
      if (!"hazard_type" %in% names(assets_with_events)) {
        assets_with_events$hazard_type <- assets_with_events[["hazard_type.event"]]
      }
      assets_with_events[["hazard_type.event"]] <- NULL
    }

    # Ensure hazard_name is preserved (needed for display in results)
    if (paste0("hazard_name", ".event") %in% names(assets_with_events)) {
      assets_with_events$hazard_name <- assets_with_events[["hazard_name.event"]]
      assets_with_events[["hazard_name.event"]] <- NULL
    }
    if (paste0("hazard_name", ".extracted") %in% names(assets_with_events)) {
      if (!"hazard_name" %in% names(assets_with_events)) {
        assets_with_events$hazard_name <- assets_with_events[["hazard_name.extracted"]]
      }
      assets_with_events[["hazard_name.extracted"]] <- NULL
    }

    # Remove the source column entirely as requested - we only care about matching_method
    # We do this for all possible variants of the source column
    source_cols <- grep("^source(\\..+)?$", names(assets_with_events), value = TRUE)
    if (length(source_cols) > 0) {
      for (scol in source_cols) {
        assets_with_events[[scol]] <- NULL
      }
    }

    # Preserve per-indicator metadata from extraction when suffixes exist
    # This must be OUTSIDE the loop to ensure it runs even if event_cols is empty
    if (paste0("hazard_indicator", ".extracted") %in% names(assets_with_events)) {
      assets_with_events$hazard_indicator <- assets_with_events[["hazard_indicator.extracted"]]
      assets_with_events[["hazard_indicator.extracted"]] <- NULL
    }
    if (paste0("hazard_indicator", ".event") %in% names(assets_with_events)) {
      assets_with_events[["hazard_indicator.event"]] <- NULL
    }

    # Backfill missing index columns (e.g., season) from inventory when absent
    # This prevents mapping joins from failing when a required index is missing
    index_backfill_cols <- intersect(
      c("season", "scenario_name", "return_period", "gwl", "ensemble"),
      names(hazards_inventory)
    )
    if (length(index_backfill_cols) > 0) {
      inventory_backfill <- hazards_inventory |>
        dplyr::select("indicator_key", dplyr::any_of(index_backfill_cols)) |>
        dplyr::distinct(.data$indicator_key, .keep_all = TRUE)

      assets_with_events <- assets_with_events |>
        dplyr::left_join(inventory_backfill, by = "indicator_key", suffix = c("", ".inv"))

      for (col in index_backfill_cols) {
        inv_col <- paste0(col, ".inv")
        if (inv_col %in% names(assets_with_events)) {
          if (col %in% names(assets_with_events)) {
            assets_with_events[[col]] <- dplyr::coalesce(assets_with_events[[col]], assets_with_events[[inv_col]])
          } else {
            assets_with_events[[col]] <- assets_with_events[[inv_col]]
          }
          assets_with_events[[inv_col]] <- NULL
        }
      }
    }

  # Step 2.4: Apply spatial separation (per event) before mapping/shock joins
  .report_progress(0.50, "Applying spatial separation...")
  spatial_eval <- evaluate_spatial_separation(
    assets_with_events = assets_with_events,
    events = events,
    hazard_configs = hazard_configs,
    spatial_separation_data = spatial_separation_data,
    base_dir = base_dir,
    adm1_boundaries = adm1_boundaries,
    adm2_boundaries = adm2_boundaries
  )

  assets_with_events <- assets_with_events |>
    dplyr::left_join(spatial_eval, by = c("asset", "event_id")) |>
    dplyr::mutate(
      spatial_included = dplyr::coalesce(.data$spatial_included, TRUE),
      spatial_multiplier = dplyr::coalesce(as.numeric(.data$spatial_multiplier), 1)
    )

  included_events <- assets_with_events |>
    dplyr::filter(.data$spatial_included)

  excluded_events <- assets_with_events |>
    dplyr::filter(!.data$spatial_included)

  excluded_rows <- build_spatial_exclusion_rows(excluded_events)

  # Step 2.5: Join mapping tables for hazard-specific factors (included rows only)
  .report_progress(0.60, "Joining damage and cost factors...")
  assets_factors_included <- if (nrow(included_events) > 0) {
    join_damage_cost_factors(included_events, hazard_configs, hazards_dir)
  } else {
    tibble::tibble()
  }

  # Rows used for shocks are the included rows only.
  assets_factors_for_shocks <- assets_factors_included

  # Assets factors output includes both included rows (with factors) and excluded rows (with status).
  # Avoid binding with empty tables to prevent class-coercion issues on complex columns.
  assets_factors <- if (nrow(assets_factors_included) == 0 && nrow(excluded_rows) == 0) {
    tibble::tibble()
  } else if (nrow(excluded_rows) == 0) {
    assets_factors_included
  } else if (nrow(assets_factors_included) == 0) {
    excluded_rows
  } else {
    dplyr::bind_rows(assets_factors_included, excluded_rows)
  }

  # Standardize output columns: keep hazard_return_period and remove long-format metadata
  if ("return_period" %in% names(assets_factors)) {
    if ("hazard_return_period" %in% names(assets_factors)) {
      assets_factors$hazard_return_period <- dplyr::coalesce(
        assets_factors$hazard_return_period,
        assets_factors$return_period
      )
    } else {
      assets_factors$hazard_return_period <- assets_factors$return_period
    }
    assets_factors$return_period <- NULL
  }

  # Drop long-format only columns and any suffixed columns
  drop_cols <- c("hazard_indicator", "hazard_intensity", "source", "indicator_key", "hazard_key")
  drop_cols <- intersect(drop_cols, names(assets_factors))
  if (length(drop_cols) > 0) {
    assets_factors <- assets_factors |>
      dplyr::select(-dplyr::all_of(drop_cols))
  }

  suffix_cols <- grep("\\.(extracted|event|inv)$", names(assets_factors), value = TRUE)
  if (length(suffix_cols) > 0) {
    assets_factors <- assets_factors |>
      dplyr::select(-dplyr::all_of(suffix_cols))
  }


  # ============================================================================
  # PHASE 3: SHOCK - Compute baseline and shocked yearly trajectories
  # ============================================================================
  .report_progress(0.70, "Computing revenue trajectories...")

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
    assets_with_factors = assets_factors_for_shocks,
    events = events,
    hazard_configs = hazard_configs,
    companies = companies
  )

  yearly_scenarios <- concatenate_baseline_and_shock(yearly_baseline, yearly_shock)

  # ============================================================================
  # PHASE 4: FINANCIAL_ASSETS - Asset-level financial computations
  # ============================================================================
  .report_progress(0.82, "Computing asset financials...")

  # Apply discounting to yearly scenarios
  assets_discounted_yearly <- discount_yearly_profits(yearly_scenarios, discount_rate)


  # ============================================================================
  # PHASE 5: FINANCIALS_COMPANY - Company-level aggregation and risk metrics
  # ============================================================================
  .report_progress(0.91, "Computing company financials...")

  # Compute company-level yearly trajectories for detailed analysis
  company_yearly_trajectories <- aggregate_assets_to_company(assets_discounted_yearly)

  # Use companies financials function that works with yearly data
  companies_result <- compute_companies_financials(companies, company_yearly_trajectories, assets_discounted_yearly, discount_rate, risk_free_rate)


  # ============================================================================
  # PHASE 6: UTILS - Final result formatting and output
  # ============================================================================
  .report_progress(1.00, "Done.")

  # Final results include both aggregated and yearly trajectory data
  final_results <- list(
    assets_factors = assets_factors,
    companies = companies_result,
    assets_yearly = assets_discounted_yearly,
    companies_yearly = company_yearly_trajectories
  )


  final_results
}
