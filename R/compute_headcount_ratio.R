compute_headcount_ratio <- function(
  .data,
  ...,
  .mpi_specs = getOption('mpi_index'),
  .cutoff_index = 1,
  .uncensored = F
) {

  cutoff <- NULL
  n <- NULL

  .data <- .data |>
    dplyr::filter(cutoff == .mpi_specs$poverty_cutoffs[.cutoff_index])

  .pattern_str <- '^d\\d{2}_i\\d{2}.*_uncensored_k$'
  if(.uncensored) .pattern_str <- '^d\\d{2}_i\\d{2}.*_uncensored$'

  df <- .data |>
    dplyr::group_by(...) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::group_by(..., n) |>
    dplyr::summarise_at(dplyr::vars(dplyr::matches(.pattern_str)), mean, na.rm = T) |>
    dplyr::select(..., n, dplyr::matches(.pattern_str))

  return(df)
}
