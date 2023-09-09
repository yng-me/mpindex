create_deprivation_matrix <- function(
  .data,
  .deprivation_cutoffs,
  ...,
  .mpi_specs = getOption('mpi_specs')
) {

  cutoff <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  if(is.null(.mpi_specs)) {
    stop('MPI specifications must be defined first.')
  }

  if(!is.null(.mpi_specs$uid)) {
    join_by <- .mpi_specs$uid
  } else {
    join_by <- 'uid'
    .data <- .data |> tibble::rownames_to_column(var = join_by)
  }

  .dep_matrix <- list()

  .dep_matrix_ref <- .data |>
    dplyr::select(!!as.name(join_by), ...) |>
    bind_list(.deprivation_cutoffs, join_by) |>
    dplyr::mutate(
      deprivation_score = rowSums(
        dplyr::across(dplyr::ends_with('_weighted')),
        na.rm = T
      )
    )

  .dep_matrix[['Uncensored']] <- .dep_matrix_ref |>
    dplyr::select(
      !!as.name(join_by),
      ...,
      deprivation_score,
      dplyr::ends_with('_unweighted')
    ) |>
    dplyr::rename_all(~ stringr::str_remove(., '_unweighted$'))


  .cutoffs <- .mpi_specs$poverty_cutoffs
  .p_cutoffs <- set_cutoff_label(.cutoffs)

  for (k in seq_along(.cutoffs)) {

    .dep_label <- set_dep_label(.p_cutoffs, k)

    .dep_matrix[[.dep_label]] <- .dep_matrix_ref |>
      dplyr::mutate(
        cutoff = .cutoffs[k],
        is_deprived = dplyr::if_else(deprivation_score >= cutoff, 1, 0)
      ) |>
      dplyr::mutate(
        deprivation_score = dplyr::if_else(is_deprived == 1, deprivation_score, 0)
      ) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::ends_with('_unweighted')),
        list(censored = ~ dplyr::if_else(is_deprived == 0, 0, .))
      ) |>
      dplyr::select(
        !!as.name(join_by),
        ...,
        cutoff,
        is_deprived,
        deprivation_score,
        dplyr::ends_with('_censored')
      ) |>
      dplyr::rename_all(~ stringr::str_remove(., '_unweighted_censored$'))
  }

  return(.dep_matrix)
}
