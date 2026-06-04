compute_headcount_ratio_adjusted <- function(.data, .aggregation = NULL, ...) {

  if (!"is_deprived" %in% names(.data)) {
    .data <- .data |>
      dplyr::mutate(is_deprived = as.integer(deprivation_score > 0))
  }

  df <- dplyr::group_by(.data, ...)

  if (!is.null(.aggregation)) {
    if (.aggregation %in% names(.data)) {
      df <- dplyr::group_by(.data, !!as.name(.aggregation), .add = TRUE)
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
    ) |>
    tsg::rename_label(
      headcount_ratio = "Headcount Ratio (H)",
      intensity = "Intensity of Deprivation Among the Poor (A)",
      mpi = "MPI (H x A)"
    )

  class(df) <- c("mpi_df", class(df))

  return(df)

}
