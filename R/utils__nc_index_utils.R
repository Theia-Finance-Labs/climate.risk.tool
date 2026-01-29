#' @noRd
nc_name_eq <- function(x, opts) any(tolower(x) == tolower(opts))

#' @noRd
nc_normalize_indexed_dim <- function(vals, mapping) {
  if (inherits(vals, "try-error")) return(vals)
  if (is.null(vals) || length(vals) == 0) return(vals)
  if ((is.integer(vals) || is.numeric(vals)) &&
    length(vals) == length(mapping) &&
    all(as.integer(vals) == seq_along(mapping))) {
    return(mapping)
  }
  vals
}
