compute_headcount_ratio <- function(.data, .aggregation = NULL, ...) {

  n <- NULL
  .pattern_str <- '^d\\d{2}_i\\d{2}.*'

  df <- .data |>
    dplyr::group_by(...) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::group_by(n, ...) |>
    dplyr::summarise_at(dplyr::vars(dplyr::matches(.pattern_str)), mean, na.rm = T) |>
    dplyr::select(n, ..., dplyr::matches(.pattern_str))


  if(!is.null(.aggregation)) {
    if(.aggregation %in% names(.data)) {
      df <- .data |>
        dplyr::group_by(!!as.name(.aggregation), ...) |>
        dplyr::add_count() |>
        dplyr::group_by(!!as.name(.aggregation), n, ...) |>
        dplyr::summarise_at(dplyr::vars(dplyr::matches(.pattern_str)), mean, na.rm = T) |>
        dplyr::select(!!as.name(.aggregation), n, ..., dplyr::matches(.pattern_str))
    }
  }

  return(df)

}
