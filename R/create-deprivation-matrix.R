create_deprivation_matrix <- function(
  .data,
  .deprivation_profile,
  ...,
  .mpi_specs = getOption("mpi_specs")
) {
  validate_mpi_specs(.mpi_specs)

  cutoff <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  spec_attr <- attributes(.mpi_specs)

  if (!(identical(
    sort(.mpi_specs$variable),
    sort(names(.deprivation_profile))
  )
  )) {
    stop("Deprivation profile is incomplete.")
  }

  if (!is.null(spec_attr$aggregation)) {
    if (!(spec_attr$aggregation %in% names(.data))) {
      stop("aggregation column defined in specification file does not exist in the dataset.")
    }
  }

  if (!is.null(spec_attr$uid)) {
    join_by <- spec_attr$uid
  } else {
    join_by <- "uid"
    .data <- .data |> tibble::rownames_to_column(var = join_by)
  }

  dep_matrix <- list()

  dep_matrix_ref <- .data |>
    dplyr::select(
      !!as.name(join_by),
      dplyr::any_of(spec_attr$aggregation),
      ...
    ) |>
    bind_list(.deprivation_profile, join_by) |>
    dplyr::mutate(
      deprivation_score = rowSums(
        dplyr::across(dplyr::ends_with("_weighted")),
        na.rm = TRUE
      )
    )

  dep_matrix[["uncensored"]] <- dep_matrix_ref |>
    dplyr::select(
      !!as.name(join_by),
      dplyr::any_of(spec_attr$aggregation),
      ...,
      deprivation_score,
      dplyr::ends_with("_unweighted")
    ) |>
    dplyr::rename_all(~ stringr::str_remove(., "_unweighted$"))


  cutoffs <- spec_attr$poverty_cutoffs
  p_cutoffs <- set_k_label(cutoffs)

  for (k in seq_along(cutoffs)) {
    dep_label <- set_dep_label(p_cutoffs, k)

    dep_matrix[[dep_label]] <- dep_matrix_ref |>
      dplyr::mutate(
        cutoff = cutoffs[k],
        is_deprived = dplyr::if_else(deprivation_score >= cutoff, 1, 0)
      ) |>
      dplyr::mutate(
        deprivation_score = dplyr::if_else(
          is_deprived == 1,
          deprivation_score,
          0
        )
      ) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::ends_with("_unweighted")),
        list(censored = ~ dplyr::if_else(is_deprived == 0, 0, .))
      ) |>
      dplyr::select(
        !!as.name(join_by),
        dplyr::any_of(spec_attr$aggregation),
        ...,
        cutoff,
        is_deprived,
        deprivation_score,
        dplyr::ends_with("_censored")
      ) |>
      dplyr::rename_all(~ stringr::str_remove(., "_unweighted_censored$"))
  }

  class(dep_matrix) <- c("mpi_deprivation_matrix", class(dep_matrix))

  return(dep_matrix)
}
