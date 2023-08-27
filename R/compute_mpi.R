#' Compute MPI
#'
#' @param .data
#' @param ...
#' @param names_separator
#'
#' @return
#' @export
#'
#' @examples
#'

compute_mpi <- function(.data, ..., names_separator = '_') {
  .data |>
    dplyr::group_by(..., cutoff) |>
    dplyr::mutate(cutoff = paste0(cutoff, '%')) |>
    dplyr::summarise(
      n = n(),
      H = (sum(is_deprived, na.rm = T)) / n,
      A = dplyr::if_else(
        sum(is_deprived, na.rm = T) == 0, 0,
        sum(censored_score, na.rm = T) * (1 / sum(is_deprived, na.rm = T))
      ),
      MPI = (1 / n) * sum(censored_score, na.rm = T),
      .groups = 'drop'
    ) |>
    dplyr::rename(
      '1 Headcount Ratio (H)' = H,
      '2 Intensity of Deprivation Among the Poor (A)' = A,
      '3 MPI (H x A)' = MPI
    ) |>
    tidyr::pivot_wider(
      names_from = cutoff,
      values_from = c(
        `1 Headcount Ratio (H)`,
        `2 Intensity of Deprivation Among the Poor (A)`,
        `3 MPI (H x A)`
      ),
      values_fill = 0,
      names_sep = paste0(' Cutoff', names_separator)
    )
}
