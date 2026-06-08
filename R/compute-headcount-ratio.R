compute_headcount_ratio <- function(
  .data,
  aggregation   = NULL,
  ...,
  survey_design = NULL,
  inference     = FALSE,
  ci_level      = 0.95
) {

  pattern_str <- "^d\\d{2}_i\\d{2}.*"

  # --- Survey path --------------------------------------------------------
  if (!is.null(survey_design)) {
    dots     <- rlang::quos(...)
    by_names <- vapply(dots, rlang::as_label, character(1))
    agg      <- if (!is.null(aggregation) && aggregation %in% names(.data)) aggregation else NULL
    all_by   <- unique(c(agg, by_names))

    ind_cols <- grep(pattern_str, names(.data), value = TRUE)
    missing  <- ind_cols[!ind_cols %in% names(survey_design$variables)]
    if (length(missing) > 0) {
      uid_col <- intersect(names(survey_design$variables), names(.data))
      uid_col <- uid_col[!uid_col %in% c("is_deprived", "deprivation_score")][1]
      survey_design <- design_with_dm(survey_design, .data, uid_col)
    }

    df <- svy_compute_headcount_ratio(survey_design, all_by, pattern_str, inference, ci_level)
    class(df) <- c("mpi_headcount_ratio", class(df))
    return(df)
  }

  # --- dplyr path ---------------------------------------------------------
  df <- .data |>
    dplyr::group_by(...) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::group_by(n, ...) |>
    dplyr::summarise(
      dplyr::across(dplyr::matches(pattern_str), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::select(n, ..., dplyr::matches(pattern_str))

  if (!is.null(aggregation)) {
    if (aggregation %in% names(.data)) {
      df <- .data |>
        dplyr::group_by(!!as.name(aggregation), ...) |>
        dplyr::add_count() |>
        dplyr::group_by(!!as.name(aggregation), n, ...) |>
        dplyr::summarise(
          dplyr::across(dplyr::matches(pattern_str), \(x) mean(x, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::select(!!as.name(aggregation), n, ..., dplyr::matches(pattern_str))
    }
  }

  class(df) <- c("mpi_headcount_ratio", class(df))
  return(df)
}
