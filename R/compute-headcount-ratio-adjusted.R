compute_headcount_ratio_adjusted <- function(.data, .aggregation = NULL, ...) {
  n <- NULL
  mpi <- NULL
  headcount_ratio <- NULL
  intensity <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  df <- .data |>
    dplyr::group_by(...)

  if (!is.null(.aggregation)) {
    if (.aggregation %in% names(.data)) {
      df <- .data |>
        dplyr::group_by(!!as.name(.aggregation), ...)
    }
  }

  df <- df |>
    dplyr::summarise(
      n = dplyr::n(),
      headcount_ratio = (sum(is_deprived, na.rm = T)) / n,
      intensity = dplyr::if_else(
        sum(is_deprived, na.rm = T) == 0, 0,
        sum(deprivation_score, na.rm = T) * (1 / sum(is_deprived, na.rm = T))
      ),
      mpi = headcount_ratio * intensity, # OR, MPI = (1 / n) * sum(censored_score, na.rm = T),
      .groups = "drop"
    )


  attr(df$headcount_ratio, "label") <- "Headcount Ratio (H)"
  attr(df$intensity, "label") <- "Intensity of Deprivation Among the Poor (A)"
  attr(df$mpi, "label") <- "MPI (H x A)"

  class(df) <- c("mpi_df", class(df))

  return(df)

}
