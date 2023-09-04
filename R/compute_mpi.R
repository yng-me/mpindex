#' Compute MPI
#'
#' @param ... Grouping columns
#' @param .mpi_specs A data frame or tibble containing specifications of the MPI
#' @param .names_separator Column separator that defines the hierarchy of the column header.
#' @param .deprivation_matrix Deprivation matrix
#' @param .generate_output Whether to generate an output
#' @param .output_filename Output filename
#'
#' @return
#' @export
#'
#' @examples

compute_mpi <- function(
  .deprivation_matrix,
  ...,
  .mpi_specs = getOption('mpi_specs'),
  .names_separator = '>',
  .generate_output = FALSE,
  .output_filename = NULL
) {

  n <- NULL
  H <- NULL
  A <- NULL
  MPI <- NULL
  cutoff <- NULL
  is_deprived <- NULL
  censored_score <- NULL
  uncensored_score <- NULL
  `1 Headcount Ratio (H)` <- NULL
  `2 Intensity of Deprivation Among the Poor (A)` <- NULL
  `3 MPI (H x A)` <- NULL

  .p_cutoffs <- paste0(round(.mpi_specs$poverty_cutoffs * 100), '%')

  # Headcount ratio ------------------------------------------------------------
  .hr_list <- list()
  .hr_list[['Uncensored']] <- .deprivation_matrix |>
    compute_headcount_ratio(
      ...,
      .mpi_specs = .mpi_specs,
      .cutoff_index = 1,
      .uncensored = T
    ) |>
    rename_indicators(.mpi_specs = .mpi_specs)

  for (i in seq_along(.p_cutoffs)) {
    .hr_list[[.p_cutoffs[i]]] <- .deprivation_matrix |>
      compute_headcount_ratio(
        ...,
        .mpi_specs = .mpi_specs,
        .cutoff_index = i
      ) |>
      rename_indicators(.mpi_specs = .mpi_specs)
  }

  # MPI ------------------------------------------------------------------------
  .mpi_computed <- .deprivation_matrix |>
    dplyr::group_by(..., cutoff) |>
    dplyr::mutate(cutoff = paste0(round(cutoff * 100), '%')) |>
    dplyr::summarise(
      n = dplyr::n(),
      H = (sum(is_deprived, na.rm = T)) / n,
      A = dplyr::if_else(
        sum(is_deprived, na.rm = T) == 0, 0,
        sum(censored_score, na.rm = T) * (1 / sum(is_deprived, na.rm = T))
      ),
      MPI = H * A, # OR, MPI = (1 / n) * sum(censored_score, na.rm = T),
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
      names_glue = '{cutoff} >>>{.value}'
    ) |>
    dplyr::rename_all(~ stringr::str_replace(
        .,
        '>>>\\d{1} ',
        paste0('cutoff', .names_separator)
      )
    )


  # MPI contribution -----------------------------------------------------------
  .contribution_list <- list()

  for (j in seq_along(.p_cutoffs)) {

    M_0_names <- paste0(
      .p_cutoffs[j],
      ' cutoff',
      .names_separator,
      'MPI (H x A)'
    )

    .contribution_list[[.p_cutoffs[j]]] <- .mpi_computed |>
      dplyr::select(M_0 = !!as.name(M_0_names)) |>
      dplyr::bind_cols(.hr_list[[.p_cutoffs[j]]]) |>
      compute_contribution(..., .mpi_specs = .mpi_specs)
  }

  if(.generate_output) {

    if(is.null(.output_filename)) {
      .output_filename <- 'MPI'
    }

  }

  return(
    list(
      mpi = .mpi_computed,
      contribution = .contribution_list,
      headcount_ratio = .hr_list
    )
  )
}



