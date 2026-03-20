#!/usr/bin/env Rscript

library(readxl)
library(readr)
library(dplyr)
library(sf)
library(stringi)

INPUT_FILE <- "workspace/brazil_area_codes/DTB_2024/RELATORIO_DTB_BRASIL_2024_DISTRITOS.xls"
OUTPUT_FILE <- "workspace/brazil_area_codes/brazil_area_codes.csv"
ADM_OUTPUT_FILE <- "workspace/brazil_area_codes/brazil_adm_codes.csv"
MUNICIPALITY_SHAPEFILE <- "tests/tests_data/areas/municipality/geoBoundaries-BRA-ADM2_simplified.geojson"
STATE_SHAPEFILE <- "tests/tests_data/areas/state/geoBoundaries-BRA-ADM1_simplified.geojson"
PROJECTED_CRS <- 5880L
BRAZIL_AREA_COL_TYPES <- rep("text", 25)

normalize_name <- function(x) {
  tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
}

fix_state_shape_names <- function(x) {
  dplyr::case_when(
    x == "Rio de Jeneiro" ~ "Rio de Janeiro",
    x == "Rio Granda do Norte" ~ "Rio Grande do Norte",
    TRUE ~ x
  )
}

fix_municipality_shape_names <- function(x) {
  dplyr::case_when(
    x == "Gracho Cardoso" ~ "Graccho Cardoso",
    x == "Barão de Monte Alto" ~ "Barão do Monte Alto",
    x == "Pingo d'Água" ~ "Pingo-d'Água",
    x == "Grão Pará" ~ "Grão-Pará",
    x == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
    TRUE ~ x
  )
}

canonicalize_municipality_normalized_name <- function(x) {
  normalized_name <- normalize_name(x)

  dplyr::case_when(
    normalized_name == "gracho cardoso" ~ "graccho cardoso",
    normalized_name == "barao de monte alto" ~ "barao do monte alto",
    normalized_name == "pingo d'agua" ~ "pingo-d'agua",
    normalized_name == "grao para" ~ "grao-para",
    normalized_name == "santo antonio do leverger" ~ "santo antonio de leverger",
    TRUE ~ normalized_name
  )
}

format_diagnostics <- function(diagnostics, max_rows = 20) {
  if (nrow(diagnostics) == 0) {
    return("<no diagnostics>")
  }

  printable <- diagnostics %>%
    dplyr::select(dplyr::any_of(c("code", "name", "uf_code", "reason"))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tibble::as_tibble()

  if (nrow(printable) > max_rows) {
    printable <- printable[seq_len(max_rows), , drop = FALSE]
  }

  paste(capture.output(print(printable, n = max_rows)), collapse = "\n")
}

abort_with_diagnostics <- function(message, diagnostics) {
  stop(
    paste(
      message,
      "Diagnostics (first rows):",
      format_diagnostics(diagnostics),
      sep = "\n"
    ),
    call. = FALSE
  )
}

warn_with_diagnostics <- function(message, diagnostics) {
  warning(
    paste(
      message,
      "Diagnostics (first rows):",
      format_diagnostics(diagnostics),
      sep = "\n"
    ),
    call. = FALSE
  )
}

read_brazil_area_data <- function(input_file) {
  readxl::read_excel(
    path = input_file,
    skip = 6,
    col_names = TRUE,
    col_types = BRAZIL_AREA_COL_TYPES
  )
}

extract_adm1_data <- function(data) {
  tibble::tibble(
    code = as.character(data[[1]]),
    name = as.character(data[[2]])
  ) %>%
    dplyr::distinct(.data$code, .data$name) %>%
    dplyr::mutate(
      adm = "adm1",
      normalized_name = normalize_name(.data$name)
    ) %>%
    dplyr::arrange(.data$code)
}

extract_adm2_data <- function(data) {
  tibble::tibble(
    uf_code = as.character(data[[1]]),
    uf_name = as.character(data[[2]]),
    code = as.character(data[[8]]),
    name = as.character(data[[9]])
  ) %>%
    dplyr::distinct(.data$uf_code, .data$uf_name, .data$code, .data$name) %>%
    dplyr::mutate(
      adm = "adm2",
      normalized_name = canonicalize_municipality_normalized_name(.data$name)
    ) %>%
    dplyr::arrange(.data$code)
}

validate_adm2_source <- function(adm2_data) {
  invalid_codes <- adm2_data %>%
    dplyr::filter(substr(.data$code, 1, 2) != .data$uf_code) %>%
    dplyr::transmute(
      code = .data$code,
      name = .data$name,
      uf_code = .data$uf_code,
      reason = "municipality code prefix does not match UF code"
    )

  if (nrow(invalid_codes) > 0) {
    abort_with_diagnostics("Found ADM2 rows with invalid UF prefixes.", invalid_codes)
  }
}

read_state_shapes <- function(state_shapefile, adm1_data) {
  if (!file.exists(state_shapefile)) {
    stop("State shapefile not found at ", state_shapefile, call. = FALSE)
  }

  state_lookup <- adm1_data %>%
    dplyr::transmute(
      uf_code = .data$code,
      normalized_name = .data$normalized_name
    )

  state_shapes <- sf::st_read(state_shapefile, quiet = TRUE) %>%
    dplyr::mutate(
      shapeName = fix_state_shape_names(.data$shapeName),
      normalized_name = normalize_name(.data$shapeName)
    ) %>%
    dplyr::left_join(state_lookup, by = "normalized_name")

  missing_state_codes <- state_shapes %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(is.na(.data$uf_code)) %>%
    dplyr::transmute(
      code = NA_character_,
      name = .data$shapeName,
      uf_code = NA_character_,
      reason = "state shapefile name could not be mapped to a UF code"
    )

  if (nrow(missing_state_codes) > 0) {
    abort_with_diagnostics("State shapefile contains unmapped states.", missing_state_codes)
  }

  duplicated_states <- state_shapes %>%
    sf::st_drop_geometry() %>%
    dplyr::count(.data$uf_code, sort = TRUE) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::transmute(
      code = NA_character_,
      name = NA_character_,
      uf_code = .data$uf_code,
      reason = "multiple state shapes mapped to the same UF code"
    )

  if (nrow(duplicated_states) > 0) {
    abort_with_diagnostics("State shapefile UF mapping is ambiguous.", duplicated_states)
  }

  state_shapes
}

resolve_state_assignment_shapefile <- function(state_shapefile) {
  preferred_shapefile <- sub("_simplified\\.geojson$", ".shp", state_shapefile)

  if (!identical(preferred_shapefile, state_shapefile) && file.exists(preferred_shapefile)) {
    return(preferred_shapefile)
  }

  state_shapefile
}

match_adm1_shape_ids <- function(adm1_data, state_shapes) {
  state_lookup <- state_shapes %>%
    sf::st_drop_geometry() %>%
    dplyr::select("normalized_name", "shapeID")

  matched_adm1 <- adm1_data %>%
    dplyr::left_join(state_lookup, by = "normalized_name")

  unmatched_adm1 <- matched_adm1 %>%
    dplyr::filter(is.na(.data$shapeID)) %>%
    dplyr::transmute(
      code = .data$code,
      name = .data$name,
      uf_code = .data$code,
      reason = "no matching ADM1 shapeID"
    )

  if (nrow(unmatched_adm1) > 0) {
    abort_with_diagnostics("ADM1 matching failed for one or more states.", unmatched_adm1)
  }

  matched_adm1
}

compute_overlap_area <- function(municipality_geom, state_geom) {
  intersection <- suppressWarnings(sf::st_intersection(municipality_geom, state_geom))

  if (nrow(intersection) == 0) {
    return(0)
  }

  sum(as.numeric(sf::st_area(intersection)))
}

read_municipality_shapes <- function(municipality_shapefile) {
  if (!file.exists(municipality_shapefile)) {
    stop("Municipality shapefile not found at ", municipality_shapefile, call. = FALSE)
  }

  sf::st_read(municipality_shapefile, quiet = TRUE) %>%
    dplyr::mutate(
      shapeName = fix_municipality_shape_names(.data$shapeName),
      normalized_name = canonicalize_municipality_normalized_name(.data$shapeName)
    )
}

build_unique_municipality_lookup <- function(adm2_data, municipality_shapes) {
  source_unique_names <- adm2_data %>%
    dplyr::count(.data$normalized_name, name = "source_count") %>%
    dplyr::filter(.data$source_count == 1) %>%
    dplyr::select("normalized_name")

  municipality_unique_lookup <- municipality_shapes %>%
    sf::st_drop_geometry() %>%
    dplyr::semi_join(source_unique_names, by = "normalized_name") %>%
    dplyr::transmute(
      name = .data$shapeName,
      shapeID = .data$shapeID,
      normalized_name = .data$normalized_name
    )

  duplicate_shape_names <- municipality_unique_lookup %>%
    dplyr::count(.data$normalized_name, name = "shape_count") %>%
    dplyr::filter(.data$shape_count > 1)

  if (nrow(duplicate_shape_names) > 0) {
    diagnostics <- municipality_unique_lookup %>%
      dplyr::semi_join(duplicate_shape_names, by = "normalized_name") %>%
      dplyr::transmute(
        code = NA_character_,
        name = .data$name,
        uf_code = NA_character_,
        reason = "multiple shapefile rows found for a source-unique municipality name"
      )

    abort_with_diagnostics("Unique municipality names are ambiguous in the shapefile.", diagnostics)
  }

  missing_unique_names <- source_unique_names %>%
    dplyr::anti_join(municipality_unique_lookup, by = "normalized_name") %>%
    dplyr::transmute(
      code = NA_character_,
      name = .data$normalized_name,
      uf_code = NA_character_,
      reason = "source-unique municipality name not found in shapefile"
    )

  if (nrow(missing_unique_names) > 0) {
    warn_with_diagnostics("Some unique municipality names could not be matched.", missing_unique_names)
  }

  municipality_unique_lookup
}

generate_permutations <- function(values) {
  if (length(values) == 1) {
    return(list(values))
  }

  permutations <- vector("list", 0)

  for (value_index in seq_along(values)) {
    remaining_values <- values[-value_index]
    remaining_permutations <- generate_permutations(remaining_values)
    permutations <- c(
      permutations,
      lapply(remaining_permutations, function(remaining_permutation) {
        c(values[[value_index]], remaining_permutation)
      })
    )
  }

  permutations
}

compute_state_assignment_score <- function(municipality_geom, state_geom) {
  overlap_area <- compute_overlap_area(municipality_geom, state_geom)

  if (overlap_area > 0) {
    return(overlap_area)
  }

  municipality_point <- suppressWarnings(sf::st_point_on_surface(municipality_geom))
  -as.numeric(sf::st_distance(municipality_point, state_geom))
}

assign_duplicate_name_group <- function(shape_group, candidate_uf_codes, state_shapes_proj) {
  if (nrow(shape_group) != length(candidate_uf_codes)) {
    diagnostics <- shape_group %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        code = NA_character_,
        name = .data$shapeName,
        uf_code = NA_character_,
        reason = paste0(
          "shapefile group size (", nrow(shape_group),
          ") does not match source duplicate count (", length(candidate_uf_codes), ")"
        )
      )

    abort_with_diagnostics("Duplicate municipality counts differ between source and shapefile.", diagnostics)
  }

  score_matrix <- matrix(
    NA_real_,
    nrow = nrow(shape_group),
    ncol = length(candidate_uf_codes),
    dimnames = list(shape_group$shapeID, candidate_uf_codes)
  )

  for (shape_index in seq_len(nrow(shape_group))) {
    municipality_geom <- shape_group[shape_index, ]

    for (uf_index in seq_along(candidate_uf_codes)) {
      state_geom <- state_shapes_proj %>%
        dplyr::filter(.data$uf_code == candidate_uf_codes[[uf_index]])

      score_matrix[shape_index, uf_index] <- compute_state_assignment_score(municipality_geom, state_geom)
    }
  }

  permutations <- generate_permutations(seq_along(candidate_uf_codes))
  permutation_scores <- vapply(
    permutations,
    function(permutation) {
      sum(score_matrix[cbind(seq_len(nrow(score_matrix)), permutation)])
    },
    numeric(1)
  )

  best_score <- max(permutation_scores)
  best_permutations <- which(abs(permutation_scores - best_score) < 1e-6)

  if (length(best_permutations) != 1) {
    diagnostics <- shape_group %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        code = NA_character_,
        name = .data$shapeName,
        uf_code = NA_character_,
        reason = "duplicate municipality state assignment is ambiguous"
      )

    abort_with_diagnostics("Could not uniquely assign duplicate municipality names to UF codes.", diagnostics)
  }

  candidate_uf_codes[permutations[[best_permutations[[1]]]]]
}

build_duplicate_municipality_lookup <- function(adm2_data, municipality_shapes, state_shapes) {
  duplicate_source <- adm2_data %>%
    dplyr::count(.data$normalized_name, name = "source_count") %>%
    dplyr::filter(.data$source_count > 1) %>%
    dplyr::select("normalized_name") %>%
    dplyr::inner_join(adm2_data, by = "normalized_name")

  if (nrow(duplicate_source) == 0) {
    return(tibble::tibble(
      uf_code = character(),
      name = character(),
      shapeID = character(),
      normalized_name = character()
    ))
  }

  duplicate_names <- duplicate_source %>%
    dplyr::distinct(.data$normalized_name) %>%
    dplyr::pull(.data$normalized_name)

  municipality_duplicates <- municipality_shapes %>%
    dplyr::filter(.data$normalized_name %in% duplicate_names)

  shape_count_mismatches <- duplicate_source %>%
    dplyr::count(.data$normalized_name, name = "source_count") %>%
    dplyr::left_join(
      municipality_duplicates %>%
        sf::st_drop_geometry() %>%
        dplyr::count(.data$normalized_name, name = "shape_count"),
      by = "normalized_name"
    ) %>%
    dplyr::filter(.data$source_count != .data$shape_count)

  if (nrow(shape_count_mismatches) > 0) {
    diagnostics <- shape_count_mismatches %>%
      dplyr::transmute(
        code = NA_character_,
        name = .data$normalized_name,
        uf_code = NA_character_,
        reason = paste0(
          "source duplicate count is ", .data$source_count,
          " but shapefile count is ", .data$shape_count
        )
      )

    abort_with_diagnostics("Duplicate municipality group counts do not align between source and shapefile.", diagnostics)
  }

  municipality_duplicates_proj <- sf::st_transform(municipality_duplicates, PROJECTED_CRS)
  state_shapes_proj <- sf::st_transform(state_shapes, PROJECTED_CRS)
  lookup_rows <- vector("list", length(duplicate_names))

  for (name_index in seq_along(duplicate_names)) {
    duplicate_name <- duplicate_names[[name_index]]
    source_group <- duplicate_source %>%
      dplyr::filter(.data$normalized_name == duplicate_name) %>%
      dplyr::distinct(.data$uf_code) %>%
      dplyr::arrange(.data$uf_code)
    shape_group <- municipality_duplicates_proj %>%
      dplyr::filter(.data$normalized_name == duplicate_name)

    assigned_uf_codes <- assign_duplicate_name_group(
      shape_group,
      source_group$uf_code,
      state_shapes_proj
    )

    lookup_rows[[name_index]] <- shape_group %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        uf_code = assigned_uf_codes,
        name = .data$shapeName,
        shapeID = .data$shapeID,
        normalized_name = .data$normalized_name
      )
  }

  municipality_duplicate_lookup <- dplyr::bind_rows(lookup_rows)

  duplicate_keys <- municipality_duplicate_lookup %>%
    dplyr::count(.data$uf_code, .data$normalized_name, sort = TRUE) %>%
    dplyr::filter(.data$n > 1)

  if (nrow(duplicate_keys) > 0) {
    ambiguous_lookup <- municipality_duplicate_lookup %>%
      dplyr::semi_join(duplicate_keys, by = c("uf_code", "normalized_name")) %>%
      dplyr::transmute(
        code = NA_character_,
        name = .data$name,
        uf_code = .data$uf_code,
        reason = "duplicate municipality lookup key in shapefile"
      )

    abort_with_diagnostics("Municipality shapefile lookup is ambiguous.", ambiguous_lookup)
  }

  municipality_duplicate_lookup
}

match_adm2_shape_ids <- function(adm2_data, municipality_shapefile, state_shapes) {
  municipality_shapes <- read_municipality_shapes(municipality_shapefile)
  name_counts <- adm2_data %>%
    dplyr::count(.data$normalized_name, name = "source_count")

  unique_names <- name_counts %>%
    dplyr::filter(.data$source_count == 1) %>%
    dplyr::select("normalized_name")
  duplicate_names <- name_counts %>%
    dplyr::filter(.data$source_count > 1) %>%
    dplyr::select("normalized_name")

  unique_lookup <- build_unique_municipality_lookup(adm2_data, municipality_shapes)
  duplicate_lookup <- build_duplicate_municipality_lookup(adm2_data, municipality_shapes, state_shapes)

  matched_unique <- adm2_data %>%
    dplyr::semi_join(unique_names, by = "normalized_name") %>%
    dplyr::left_join(
      unique_lookup %>% dplyr::select("normalized_name", "shapeID"),
      by = "normalized_name"
    )

  matched_duplicates <- adm2_data %>%
    dplyr::semi_join(duplicate_names, by = "normalized_name") %>%
    dplyr::left_join(
      duplicate_lookup %>% dplyr::select("uf_code", "normalized_name", "shapeID"),
      by = c("uf_code", "normalized_name")
    )

  matched_adm2 <- dplyr::bind_rows(matched_unique, matched_duplicates) %>%
    dplyr::arrange(.data$code)

  unmatched_adm2 <- matched_adm2 %>%
    dplyr::filter(is.na(.data$shapeID)) %>%
    dplyr::transmute(
      code = .data$code,
      name = .data$name,
      uf_code = .data$uf_code,
      reason = "no matching ADM2 shapeID"
    )

  if (nrow(unmatched_adm2) > 0) {
    warn_with_diagnostics("ADM2 matching failed for one or more municipalities.", unmatched_adm2)
  }

  duplicate_shape_ids <- matched_adm2 %>%
    dplyr::count(.data$shapeID, sort = TRUE) %>%
    dplyr::filter(.data$n > 1)

  if (nrow(duplicate_shape_ids) > 0) {
    duplicated_rows <- matched_adm2 %>%
      dplyr::semi_join(duplicate_shape_ids, by = "shapeID") %>%
      dplyr::transmute(
        code = .data$code,
        name = .data$name,
        uf_code = .data$uf_code,
        reason = paste0("duplicate ADM2 shapeID assignment: ", .data$shapeID)
      )

    abort_with_diagnostics("ADM2 matching produced duplicate shapeIDs.", duplicated_rows)
  }

  matched_adm2
}

build_adm_output <- function(raw_data, municipality_shapefile, state_shapefile) {
  adm1_data <- extract_adm1_data(raw_data)
  adm2_data <- extract_adm2_data(raw_data)

  validate_adm2_source(adm2_data)

  state_shapes <- read_state_shapes(state_shapefile, adm1_data)
  state_assignment_shapes <- read_state_shapes(
    resolve_state_assignment_shapefile(state_shapefile),
    adm1_data
  )

  matched_adm1 <- match_adm1_shape_ids(adm1_data, state_shapes)
  matched_adm2 <- match_adm2_shape_ids(adm2_data, municipality_shapefile, state_assignment_shapes)

  dplyr::bind_rows(
    matched_adm1 %>% dplyr::select("code", "name", "adm", "shapeID"),
    matched_adm2 %>% dplyr::select("code", "name", "adm", "shapeID")
  ) %>%
    dplyr::arrange(.data$adm, .data$code)
}

main <- function() {
  cat("Reading Excel file...\n")
  raw_data <- read_brazil_area_data(INPUT_FILE)

  cat("Writing full data to CSV...\n")
  readr::write_csv(raw_data, OUTPUT_FILE)
  cat(sprintf("Successfully converted %s to %s\n", INPUT_FILE, OUTPUT_FILE))
  cat(sprintf("Rows: %d, Columns: %d\n", nrow(raw_data), ncol(raw_data)))

  adm1_data <- extract_adm1_data(raw_data)
  adm2_data <- extract_adm2_data(raw_data)
  cat(sprintf("\nExtracted %d distinct adm1 codes\n", nrow(adm1_data)))
  cat(sprintf("Extracted %d distinct adm2 codes\n", nrow(adm2_data)))

  cat("\nMatching with shapefiles and adding shapeID columns...\n")
  adm_output <- build_adm_output(raw_data, MUNICIPALITY_SHAPEFILE, STATE_SHAPEFILE)

  readr::write_csv(adm_output, ADM_OUTPUT_FILE)
  cat(sprintf("\nSaved combined adm codes to %s\n", ADM_OUTPUT_FILE))
  cat(sprintf(
    "Total rows: %d (adm1: %d, adm2: %d)\n",
    nrow(adm_output),
    sum(adm_output$adm == "adm1"),
    sum(adm_output$adm == "adm2")
  ))
  cat(sprintf(
    "ADM2 shapeIDs are unique: %s\n",
    ifelse(
      nrow(dplyr::filter(adm_output, .data$adm == "adm2")) ==
        dplyr::n_distinct(dplyr::filter(adm_output, .data$adm == "adm2")$shapeID),
      "yes",
      "no"
    )
  ))
  cat("\nAll conversions completed successfully!\n")
}

  main()
