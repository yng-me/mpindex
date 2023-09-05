#' Compute MPI
#'
#' @param .data A data frame
#' @param .deprivation_cutoffs list of deprivation cutoffs
#' @param ... Grouping columns
#' @param .mpi_specs A data frame or tibble containing specifications of the MPI
#' @param .names_separator Column separator that defines the hierarchy of the column header.
#' @param .generate_output Whether to generate an output
#' @param .output_filename Output filename
#' @param .include_deprivation_matrix Include deprivation matrix
#' @param .formatted_output Whether formatting is to be applied to the output
#'
#' @return
#' @export
#'
#' @examples

compute_mpi <- function(
  .data,
  .deprivation_cutoffs,
  ...,
  .mpi_specs = getOption('mpi_specs'),
  .names_separator = '>',
  .include_deprivation_matrix = TRUE,
  .generate_output = FALSE,
  .formatted_output = TRUE,
  .output_filename = NULL
) {

  n <- NULL
  H <- NULL
  A <- NULL
  MPI <- NULL
  cutoff <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  .cutoffs <- .mpi_specs$poverty_cutoffs
  .p_cutoffs <- set_cutoff_label(.cutoffs)

  .incidence_list <- list()
  .mpi_computed_list <- list()
  .contribution_list <- list()

  .deprivation_matrix <- .data |>
    create_deprivation_matrix(
      ...,
      .deprivation_cutoffs,
      .mpi_specs = .mpi_specs
    )

  # Incidence of Poverty -------------------------------------------------------
  .incidence_list[['Uncensored']] <- .deprivation_matrix$Uncensored |>
    compute_incidence(...) |>
    rename_indicators(.mpi_specs = .mpi_specs)

  for(i in seq_along(.p_cutoffs)) {

    .dep_label <- set_dep_label(.p_cutoffs, i)
    .dm_temp <- .deprivation_matrix[[.dep_label]]

    .incidence_temp <- .dm_temp |>
      compute_incidence(...)

    .incidence_list[[.dep_label]] <- .incidence_temp |>
      rename_indicators(.mpi_specs = .mpi_specs)

    .mpi_computed_temp <- .dm_temp |>
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

    .mpi_computed_list[[.dep_label]] <- .mpi_computed_temp |>
      dplyr::rename(
        'Headcount Ratio (H)' = H,
        'Intensity of Deprivation Among the Poor (A)' = A,
        'MPI (H x A)' = MPI
      ) |>
      rename_n(.mpi_specs$unit_of_analysis)

    .contribution_list[[.dep_label]] <- .mpi_computed_temp |>
      dplyr::select(MPI) |>
      dplyr::bind_cols(.incidence_temp) |>
      compute_contribution(..., .mpi_specs = .mpi_specs)
  }


  if(.generate_output) {
    if(is.null(.output_filename)) {
      .output_filename <- 'MPI'
    }
  }

  if(length(.p_cutoffs) == 1) {
    .mpi_computed_list <- .mpi_computed_list[[1]]
    .contribution_list <- .contribution_list[[1]]
  }

  .output <- list(
    `MPI` = .mpi_computed_list,
    `Contribution of dimension` = .contribution_list,
    `Incidence of deprivation` = .incidence_list
  )


  if(.include_deprivation_matrix) {
    .dm_n <- names(.deprivation_matrix)

    if(!is.null(.mpi_specs$uid)) {
      join_by <- .mpi_specs$uid
    } else {
      join_by <- 'uid'
    }

    .output[['Deprivation matrix']] <- stats::setNames(
      lapply(.dm_n, function(x) {
        .deprivation_matrix[[x]] |>
          dplyr::select(
            !!as.name(join_by),
            ...,
            dplyr::any_of('deprivation_score'),
            dplyr::matches('^d\\d{2}_i\\d{2}.*')
          ) |>
          rename_indicators(.mpi_specs = .mpi_specs)
      }),
    .dm_n
    )
  }

  return(.output)
}



