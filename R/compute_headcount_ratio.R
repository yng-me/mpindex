compute_headcount_ratio <- function(.data, ...) {

  n <- NULL
  .pattern_str <- '^d\\d{2}_i\\d{2}.*'

  df <- .data |>
    dplyr::group_by(...) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::group_by(..., n) |>
    dplyr::summarise_at(dplyr::vars(dplyr::matches(.pattern_str)), mean, na.rm = T) |>
    dplyr::select(..., n, dplyr::matches(.pattern_str))

  return(df)
}
