compute_headcount_ratio_adj <- function(.data, ...) {

  n <- NULL
  H <- NULL
  A <- NULL
  MPI <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  .data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(),
      H = (sum(is_deprived, na.rm = T)) / n,
      A = dplyr::if_else(
        sum(is_deprived, na.rm = T) == 0, 0,
        sum(deprivation_score, na.rm = T) * (1 / sum(is_deprived, na.rm = T))
      ),
      MPI = H * A, # OR, MPI = (1 / n) * sum(censored_score, na.rm = T),
      .groups = 'drop'
    )
}
