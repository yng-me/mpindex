compute_headcount_ratio_adjusted <- function(
  .data,
  aggregation   = NULL,
  ...,
  survey_design = NULL,
  inference     = FALSE,
  ci_level      = 0.95
) {

  if (!"is_deprived" %in% names(.data)) {
    .data <- .data |>
      dplyr::mutate(is_deprived = as.integer(deprivation_score > 0))
  }

  # --- Survey path --------------------------------------------------------
  if (!is.null(survey_design)) {
    dots     <- rlang::quos(...)
    by_names <- vapply(dots, rlang::as_label, character(1))
    agg      <- if (!is.null(aggregation) && aggregation %in% names(.data)) aggregation else NULL
    all_by   <- unique(c(agg, by_names))

    needed <- c("is_deprived", "deprivation_score")
    if (!all(needed %in% names(survey_design$variables))) {
      uid_col <- intersect(names(survey_design$variables), names(.data))
      uid_col <- uid_col[!uid_col %in% c("is_deprived", "deprivation_score")][1]
      survey_design <- design_with_dm(survey_design, .data, uid_col)
    }

    df <- svy_compute_mpi_summary(survey_design, all_by, inference, ci_level)
    df <- tsg::rename_label(df,
      headcount_ratio = "Headcount Ratio (H)",
      intensity       = "Intensity of Deprivation Among the Poor (A)",
      mpi             = "MPI (H x A)"
    )
    class(df) <- c("mpi_df", class(df))
    return(df)
  }

  # --- dplyr path ---------------------------------------------------------
  df <- dplyr::group_by(.data, ...)

  if (!is.null(aggregation)) {
    if (aggregation %in% names(.data)) {
      df <- dplyr::group_by(df, !!as.name(aggregation), .add = TRUE)
    }
  }

  df <- df |>
    dplyr::summarise(
      n = dplyr::n(),
      headcount_ratio = (sum(is_deprived, na.rm = TRUE)) / n,
      intensity = dplyr::if_else(
        sum(is_deprived, na.rm = TRUE) == 0, 0,
        sum(deprivation_score, na.rm = TRUE) * (1 / sum(is_deprived, na.rm = TRUE))
      ),
      mpi = headcount_ratio * intensity,
      .groups = "drop"
    ) |>
    tsg::rename_label(
      headcount_ratio = "Headcount Ratio (H)",
      intensity       = "Intensity of Deprivation Among the Poor (A)",
      mpi             = "MPI (H x A)"
    )

  class(df) <- c("mpi_df", class(df))
  return(df)
}
