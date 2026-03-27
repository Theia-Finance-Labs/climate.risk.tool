#' Read damage and cost factors from CSV file
#'
#' @title Read damage and cost factors lookup table
#' @description Reads damage and cost factors from CSV file, parsing numeric columns
#'   and handling comma decimal separators correctly.
#' @param base_dir Character string specifying the base directory containing hazards/mappings/damage_and_cost_factors.csv
#' @return tibble with damage and cost factors
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' factors <- read_damage_cost_factors(base_dir)
#' }
#' @export
read_damage_cost_factors <- function(base_dir) {
  message("[read_damage_cost_factors] Reading damage and cost factors from: ", base_dir)

  # Define file path - now inside hazards/mappings
  factors_path <- file.path(base_dir, "hazards", "mappings", "damage_and_cost_factors.csv")

  # Check if file exists
  if (!file.exists(factors_path)) {
    stop("Damage and cost factors file not found at: ", factors_path)
  }

  # Read the damage and cost factors CSV
  # The CSV uses comma as decimal separator and quotes around numbers
  # GWL column can contain both numeric values (e.g., "1.5") and text values (e.g., "present"),
  # so we must read it as character to preserve both
  factors_df <- readr::read_csv(
    factors_path,
    col_types = readr::cols(GWL = readr::col_character()),
    show_col_types = FALSE
  ) |>
    tibble::as_tibble() |>
    # Convert column names to snake_case for consistency
    dplyr::rename_with(to_snake_case)

  df_names <- names(factors_df)

  factors_df <- factors_df |>
    dplyr::mutate(
      # Clean up the numeric columns that have comma decimal separators and quotes (if they are characters)
      damage_factor = if (is.character(.data$damage_factor)) {
        as.numeric(gsub(",", ".", gsub('"', "", .data$damage_factor)))
      } else {
        as.numeric(.data$damage_factor)
      },
      cost_factor = if ("cost_factor" %in% df_names) {
        if (is.character(.data$cost_factor)) {
          suppressWarnings(as.numeric(gsub(",", ".", gsub('"', "", .data$cost_factor))))
        } else {
          as.numeric(.data$cost_factor)
        }
      } else {
        NA_real_
      }
    ) |>
    # Normalize state names (remove accents, convert to ASCII)
    dplyr::mutate(
      state = if ("state" %in% df_names) {
        dplyr::if_else(
          !is.na(.data$state) & .data$state != "-" & nzchar(as.character(.data$state)),
          stringi::stri_trans_general(as.character(.data$state), "Latin-ASCII"),
          .data$state
        )
      } else {
        NA_character_
      }
    )

  if (!"state_code" %in% names(factors_df)) {
    factors_df$state_code <- NA_character_
  }

  message("[read_damage_cost_factors] Loaded ", nrow(factors_df), " factor records")
  factors_df
}

#' Load mapping table from hazard config
#'
#' @title Load mapping table from config
#' @description Loads a mapping table specified in a hazard config file. This is a generalized
#'   function that replaces hardcoded mapping readers like read_cnae_labor_productivity_exposure
#'   and read_land_cover_legend. Mapping tables are defined in hazard YAML config files
#'   under the mappings section.
#' @param base_dir Character string specifying the base directory
#' @param hazard_configs Named list of hazard configs from load_hazard_configs()
#' @param hazard_type Character name of the hazard type (e.g., "Heat", "Fire")
#' @param mapping_key Character key of the mapping in the config (e.g., "cnae_exposure", "land_cover_legend")
#' @return tibble with mapping data
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' hazards_dir <- file.path(base_dir, "hazards", "config")
#' hazard_configs <- load_hazard_configs(hazards_dir)
#' cnae_exposure <- load_mapping_from_config(base_dir, hazard_configs, "Heat", "cnae_exposure")
#' }
#' @export
load_mapping_from_config <- function(base_dir, hazard_configs, hazard_type, mapping_key) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs is required")
  }
  if (!hazard_type %in% names(hazard_configs)) {
    stop("Hazard type '", hazard_type, "' not found in hazard_configs")
  }

  hazard_config <- hazard_configs[[hazard_type]]
  if (is.null(hazard_config$mappings) || !mapping_key %in% names(hazard_config$mappings)) {
    stop("Mapping '", mapping_key, "' not found in hazard config for '", hazard_type, "'")
  }

  mapping <- hazard_config$mappings[[mapping_key]]
  mappings_dir <- file.path(base_dir, "hazards", "mappings")
  table_path <- file.path(mappings_dir, mapping$file)

  if (!file.exists(table_path)) {
    stop("Mapping table not found: ", table_path)
  }

  # Read the mapping table based on file extension
  ext <- tolower(tools::file_ext(table_path))
  if (ext == "csv") {
    mapping_df <- readr::read_csv(table_path, show_col_types = FALSE) |> tibble::as_tibble()
  } else if (ext %in% c("xlsx", "xls")) {
    mapping_df <- readxl::read_excel(table_path) |> tibble::as_tibble()
  } else {
    stop("Unsupported mapping table extension: ", ext)
  }

  # Apply snake_case conversion for consistency
  mapping_df <- mapping_df |>
    dplyr::rename_with(to_snake_case)

  if ("state" %in% names(mapping_df) && !"state_code" %in% names(mapping_df)) {
    mapping_df$state_code <- NA_character_
  }
  if ("municipality" %in% names(mapping_df) && !"municipality_code" %in% names(mapping_df)) {
    mapping_df$municipality_code <- NA_character_
  }

  message("[load_mapping_from_config] Loaded mapping '", mapping_key, "' for hazard '", hazard_type, "': ", nrow(mapping_df), " records")
  mapping_df
}

#' Read precomputed administrative hazard statistics from CSV file
#'
#' @title Read precomputed hazard statistics for provinces and municipalities
#' @description Reads precomputed hazard statistics from CSV file containing hazard data
#'   aggregated at ADM1 (province) and ADM2 (municipality) levels. Used to look up hazard
#'   values for assets without coordinates but with province or municipality information.
#'   The file uses indicator metadata (`indicator_file`, `indicator_variable`) which are mapped
#'   to `hazard_type` and `hazard_indicator` using the hazard config YAML files.
#' @param base_dir Character string specifying the base directory. The function looks for precomputed_adm_indicators.csv in base_dir/hazards/
#' @param hazard_configs Optional named list of hazard configs. If NULL, they are loaded from base_dir/hazards/config.
#' @return tibble with precomputed hazard statistics including columns: adm_name, adm_code, shape_id,
#'   state_code, adm_level, scenario_name, return_period, hazard_type, hazard_indicator,
#'   hazard_name, aggregation_method, hazard_value. adm_level is "ADM1" for provinces
#'   or "ADM2" for municipalities.
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' precomputed <- read_precomputed_hazards(base_dir)
#' # Look up Amazonas province flood hazard
#' amazonas_flood <- precomputed |>
#'   dplyr::filter(adm_name == "Amazonas", adm_level == "ADM1", hazard_type == "flood")
#' }
#' @export
read_precomputed_hazards <- function(base_dir, hazard_configs = NULL) {
  message("[read_precomputed_hazards] Reading precomputed hazard statistics from: ", base_dir)

  # Define file path
  precomputed_path <- file.path(base_dir, "hazards", "precomputed_adm_indicators.csv")

  # Check if file exists
  if (!file.exists(precomputed_path)) {
    stop("Precomputed hazards file not found at: ", precomputed_path)
  }

  # Read the precomputed hazards CSV with optimized options
  # Providing col_types explicitly speeds up reading large files
  precomputed_df <- readr::read_csv(
    precomputed_path,
    col_types = readr::cols(
      adm_name = "c",
      adm_code = "c",
      shape_id = "c",
      adm_level = "c",
      gwl = "c",
      return_period = "d",
      indicator_file = "c",
      indicator_variable = "c",
      ensemble = "c",
      season = "c",
      scenario_name = "c",
      .default = "d"
    ),
    show_col_types = FALSE,
    lazy = FALSE,
    progress = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      adm_name = normalize_geo_name(.data$adm_name),
      adm_level = toupper(.data$adm_level),
      adm_code = as.character(.data$adm_code),
      shape_id = as.character(.data$shape_id),
      state_code = NA_character_
    )

  message("  File read complete (", nrow(precomputed_df), " rows). Mapping indicators from config...")

  # Load hazard configs if not provided
  if (is.null(hazard_configs)) {
    hazards_dir <- file.path(base_dir, "hazards", "config")
    hazard_configs <- load_hazard_configs(hazards_dir)
  }

  normalize_indicator_file <- function(x) {
    x <- gsub("/+$", "", as.character(x))
    tools::file_path_sans_ext(x)
  }

  indicator_registry <- lapply(names(hazard_configs), function(hazard_type) {
    hazard_config <- hazard_configs[[hazard_type]]
    lapply(names(hazard_config$indicators), function(indicator_key) {
      indicator <- hazard_config$indicators[[indicator_key]]
      tibble::tibble(
        hazard_type = hazard_type,
        hazard_indicator = indicator_key,
        indicator_file = indicator$file,
        indicator_variable = indicator$variable,
        variable = indicator$variable,
        agg = indicator$agg,
        categorical = indicator$categorical
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(indicator_file_key = normalize_indicator_file(.data$indicator_file))

  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      indicator_file_key = normalize_indicator_file(.data$indicator_file),
      scenario_name = dplyr::na_if(as.character(.data$scenario_name), "NA"),
      scenario_name = dplyr::na_if(.data$scenario_name, ""),
      scenario_name = dplyr::coalesce(.data$scenario_name, as.character(.data$gwl)),
      season = dplyr::na_if(as.character(.data$season), "NA"),
      ensemble = dplyr::na_if(as.character(.data$ensemble), "NA"),
      ensemble = dplyr::na_if(.data$ensemble, ""),
      ensemble = dplyr::coalesce(.data$ensemble, "mean")
    ) |>
    dplyr::left_join(
      indicator_registry |>
        dplyr::select(
          "indicator_file_key",
          "hazard_type",
          "hazard_indicator",
          "variable",
          "agg",
          "categorical"
        ),
      by = "indicator_file_key"
    )

  # Fallback mapping by indicator_variable if file matching failed
  is_unmapped <- is.na(precomputed_df$hazard_type)
  if (any(is_unmapped)) {
    fallback_registry <- indicator_registry |>
      dplyr::distinct(.data$indicator_variable, .data$hazard_type, .data$hazard_indicator, .data$variable, .data$agg, .data$categorical)

    fallback_df <- precomputed_df[is_unmapped, ] |>
      dplyr::left_join(
        fallback_registry,
        by = c("indicator_variable" = "indicator_variable"),
        suffix = c("", "_fallback")
      )

    precomputed_df$hazard_type[is_unmapped] <- fallback_df$hazard_type_fallback
    precomputed_df$hazard_indicator[is_unmapped] <- fallback_df$hazard_indicator_fallback
    precomputed_df$variable[is_unmapped] <- fallback_df$variable_fallback
    precomputed_df$agg[is_unmapped] <- fallback_df$agg_fallback
    precomputed_df$categorical[is_unmapped] <- fallback_df$categorical_fallback
  }

  if (any(is.na(precomputed_df$hazard_indicator))) {
    missing_rows <- precomputed_df |>
      dplyr::filter(is.na(.data$hazard_indicator)) |>
      dplyr::distinct(.data$indicator_file, .data$indicator_variable)
    stop(
      "Could not resolve hazard_indicator for: ",
      paste(
        paste0(missing_rows$indicator_file, " (", missing_rows$indicator_variable, ")"),
        collapse = ", "
      )
    )
  }

  if (any(is.na(precomputed_df$hazard_type))) {
    missing_rows <- precomputed_df |>
      dplyr::filter(is.na(.data$hazard_type)) |>
      dplyr::distinct(.data$indicator_file, .data$indicator_variable)
    stop(
      "Could not map indicator metadata to hazard config. Missing mappings for: ",
      paste(
        paste0(missing_rows$indicator_file, " (", missing_rows$indicator_variable, ")"),
        collapse = ", "
      )
    )
  }

  if (any(is.na(precomputed_df$scenario_name))) {
    stop("scenario_name is missing for some rows; ensure scenario_name or gwl is provided")
  }

  has_season <- "season" %in% names(precomputed_df)

  # 4. Optimized hazard_name construction using unique combinations
  message("  Building hazard names...")

  # Ensure variable is set (prefer config variable, fallback to indicator_variable or hazard_indicator)
  if (!"variable" %in% names(precomputed_df)) {
    precomputed_df$variable <- NA_character_
  }
  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      variable = dplyr::coalesce(.data$variable, .data$indicator_variable, .data$hazard_indicator)
    )

  distinct_cols <- c(
    "hazard_type",
    "hazard_indicator",
    "variable",
    "scenario_name",
    "gwl",
    "return_period",
    "indicator_file",
    "indicator_variable",
    "ensemble"
  )
  if (has_season) distinct_cols <- c(distinct_cols, "season")

  combos <- precomputed_df |>
    dplyr::distinct(dplyr::across(dplyr::any_of(distinct_cols)))

  get_index_dims <- function(hazard_type, hazard_indicator) {
    if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
      return(character())
    }
    indicator_cfg <- hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]
    if (is.null(indicator_cfg) || is.null(indicator_cfg$index)) {
      return(character())
    }
    index_dims <- as.character(indicator_cfg$index)
    index_dims <- index_dims[!is.na(index_dims)]
    index_dims
  }

  build_index_values <- function(row) {
    index_dims <- get_index_dims(row$hazard_type, row$hazard_indicator)
    has_index <- function(name) length(index_dims) > 0 && name %in% index_dims
    list(
      return_period = if (has_index("return_period") && !is.na(row$return_period)) row$return_period else NA_real_,
      gwl = if (has_index("gwl") && !is.na(row$gwl)) row$gwl else NA_character_,
      scenario_name = if (has_index("scenario_name") && !is.na(row$scenario_name)) row$scenario_name else NA_character_,
      season = if (has_season && has_index("season") && !is.na(row$season)) row$season else NA_character_
    )
  }

  # Construct structured hazard_name for each combination
  # Use fixed ensemble from config if specified, otherwise use actual ensemble from data
  # Also compute effective_ensemble to update the ensemble column
  combos$effective_ensemble <- purrr::pmap_chr(combos, function(...) {
    row <- list(...)

    # Get fixed ensemble from config if it exists
    effective_ensemble <- as.character(row$ensemble)
    if (!is.null(hazard_configs) && !is.null(row$hazard_type) && row$hazard_type %in% names(hazard_configs)) {
      indicator_cfg <- hazard_configs[[row$hazard_type]]$indicators[[row$hazard_indicator]]
      if (!is.null(indicator_cfg) && !is.null(indicator_cfg$fixed) && !is.null(indicator_cfg$fixed$ensemble)) {
        effective_ensemble <- as.character(indicator_cfg$fixed$ensemble)
      }
    }
    effective_ensemble
  })

  combos$indicator_key <- purrr::pmap_chr(combos, function(...) {
    row <- list(...)

    index_values <- build_index_values(row)

    build_indicator_key(
      indicator_file = row$indicator_file,
      indicator_variable = row$indicator_variable,
      index_values = index_values,
      ensemble = row$effective_ensemble
    )
  })

  combos$hazard_name <- purrr::pmap_chr(combos, function(...) {
    row <- list(...)

    index_values <- build_index_values(row)

    build_hazard_name(
      hazard_type = row$hazard_type,
      hazard_indicator = row$hazard_indicator,
      index_values = index_values,
      ensemble = row$effective_ensemble
    )
  })

  # Join names back to full data frame
  # Also update ensemble column to match effective_ensemble used in keys
  join_cols <- intersect(names(combos), names(precomputed_df))
  precomputed_df <- precomputed_df |>
    dplyr::left_join(combos |> dplyr::select(dplyr::all_of(c(join_cols, "hazard_name", "indicator_key", "effective_ensemble"))), by = join_cols) |>
    # CRITICAL: Filter out rows where ensemble doesn't match effective_ensemble
    # This happens when config has fixed: { ensemble: mean } but precomputed data has multiple ensembles
    # We only keep rows that match the effective ensemble to avoid duplicates
    dplyr::filter(.data$ensemble == .data$effective_ensemble) |>
    # Update ensemble column to match the effective_ensemble used in indicator_key
    dplyr::mutate(
      ensemble = .data$effective_ensemble,
      # hazard_key is the same as indicator_key (implementation detail)
      hazard_key = .data$indicator_key
    ) |>
    dplyr::select(-"effective_ensemble")

  precomputed_df <- precomputed_df |>
    dplyr::select(-"indicator_file_key")

  # Add default metadata columns to match inventory structure
  precomputed_final <- precomputed_df |>
    dplyr::mutate(
      scenario_name = as.character(.data$scenario_name),
      agg = NA_character_,
      categorical = FALSE
    )

  if (has_season) {
    precomputed_final$season <- as.character(precomputed_final$season)
  }

  message("[read_precomputed_hazards] Loaded ", nrow(precomputed_final), " final records")
  precomputed_final
}

#' Read TIF hazard mapping file
#'
#' @title Read TIF hazard mapping file
#' @description Reads a metadata CSV file that maps TIF filenames
#'   to hazard metadata (type, indicator, scenario_name, return period).
#' @param mapping_path Character path to a metadata CSV file
#' @return Tibble with mapping information
#' @noRd
read_hazards_mapping <- function(mapping_path) {
  if (!file.exists(mapping_path)) {
    stop("Mapping file not found: ", mapping_path)
  }

  mapping <- utils::read.csv(mapping_path, stringsAsFactors = FALSE, strip.white = TRUE)
  mapping <- tibble::as_tibble(mapping)

  # Validate required columns
  required_cols <- c(
    "hazard_file", "hazard_indicator",
    "scenario_name", "return_period"
  )
  missing_cols <- setdiff(required_cols, names(mapping))
  if (length(missing_cols) > 0) {
    stop("Mapping file missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Normalize hazard_type if present
  if ("hazard_type" %in% names(mapping)) {
    mapping <- mapping |>
      dplyr::mutate(hazard_type = as.character(.data$hazard_type))
  }

  return(mapping)
}


#' Load region name mapping dictionary
#'
#' @title Load mapping from normalized names to original names
#' @description Creates a dictionary mapping normalized (ASCII) region names to their original
#'   names with special characters. This is used to display original names in the frontend
#'   while keeping normalized names for internal processing.
#' @param base_dir Base directory containing areas subdirectory
#' @param adm1_sf Optional sf object for ADM1 boundaries (if already loaded)
#' @param adm2_sf Optional sf object for ADM2 boundaries (if already loaded)
#' @return Named list with two elements:
#'   - province: Named character vector mapping normalized province names to original names
#'   - municipality: Named character vector mapping normalized municipality names to original names
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' name_mapping <- load_region_name_mapping(base_dir)
#' # Access original name: name_mapping$province["Sao Paulo"] returns "São Paulo"
#' }
#' @export
load_region_name_mapping <- function(base_dir, adm1_sf = NULL, adm2_sf = NULL) {
  # Initialize result list
  mapping <- list(province = character(0), municipality = character(0))

  # Load province (ADM1) names
  if (!is.null(adm1_sf)) {
    provinces_sf <- adm1_sf
  } else {
    province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
    provinces_sf <- if (file.exists(province_path)) sf::st_read(province_path, quiet = TRUE) else NULL
  }

  if (!is.null(provinces_sf) && "shapeName" %in% names(provinces_sf)) {
    # Get original names
    original_names <- as.character(provinces_sf$shapeName)

    # Get normalized names (same way as used throughout the codebase)
    normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

    # Create mapping: normalized -> original
    mapping$province <- original_names
    names(mapping$province) <- normalized_names
  }

  # Load municipality (ADM2) names
  if (!is.null(adm2_sf)) {
    municipalities_sf <- adm2_sf
  } else {
    municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
    municipalities_sf <- if (file.exists(municipality_path)) sf::st_read(municipality_path, quiet = TRUE) else NULL
  }

  if (!is.null(municipalities_sf) && "shapeName" %in% names(municipalities_sf)) {
    # Get original names
    original_names <- as.character(municipalities_sf$shapeName)

    # Get normalized names (same way as used throughout the codebase)
    normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

    # Create mapping: normalized -> original
    mapping$municipality <- original_names
    names(mapping$municipality) <- normalized_names
  }

  message(
    "[load_region_name_mapping] Loaded ", length(mapping$province), " province names and ",
    length(mapping$municipality), " municipality names"
  )

  return(mapping)
}
