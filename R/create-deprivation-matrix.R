create_deprivation_matrix <- function(
  .data,
  deprivation_profile,
  ...,
  by_cols   = character(0),
  mpi_specs = NULL
) {

  validate_mpi_specs(mpi_specs)
  spec_attr <- attributes(mpi_specs)

  if (!identical(sort(mpi_specs$variable), sort(names(deprivation_profile)))) {
    stop("Deprivation profile is incomplete.")
  }

  if (!is.null(spec_attr$uid)) {
    join_by <- spec_attr$uid
  } else {
    join_by <- "uid"
    .data <- tibble::rownames_to_column(.data, var = join_by)
  }

  dep_matrix <- list()

  dep_matrix_ref <- dplyr::select(.data, !!as.name(join_by), dplyr::any_of(by_cols), ...)
  dep_matrix_ref <- bind_list(dep_matrix_ref, deprivation_profile, join_by) 
  
  dep_matrix_ref <- dplyr::mutate(
    dep_matrix_ref,
    deprivation_score = rowSums(
      dplyr::across(dplyr::ends_with("_weighted")),
      na.rm = TRUE
    )
  )

  dep_matrix_u <- dplyr::select(
    dep_matrix_ref,
    !!as.name(join_by),
    dplyr::any_of(by_cols),
    ...,
    deprivation_score,
    dplyr::ends_with("_unweighted")
  )

  dep_matrix[["uncensored"]] <- dplyr::rename_with(
    dep_matrix_u, 
    ~ sub("_unweighted$", "", .)
  )

  cutoffs   <- spec_attr$poverty_cutoffs
  p_cutoffs <- set_k_label(cutoffs)

  for (k in seq_along(cutoffs)) {
    
    dep_label <- set_dep_label(p_cutoffs, k)

    dep_matrix_k <- dplyr::mutate(
      dep_matrix_ref,
      cutoff = cutoffs[k],
      is_deprived = dplyr::if_else(deprivation_score >= cutoff, 1, 0),
      deprivation_score = dplyr::if_else(is_deprived == 1, deprivation_score, 0),
      dplyr::across(
        dplyr::ends_with("_unweighted"),
        list(censored = ~ dplyr::if_else(is_deprived == 0, 0, .))
      )
    )

    dep_matrix_k <- dplyr::select(
      dep_matrix_k,
      !!as.name(join_by),
      dplyr::any_of(by_cols),
      ...,
      cutoff,
      is_deprived,
      deprivation_score,
      dplyr::ends_with("_censored")
    )
  
    dep_matrix[[dep_label]] <- dplyr::rename_with(
      dep_matrix_k, 
      ~ sub("_unweighted_censored$", "", .)
    )

  }

  class(dep_matrix) <- c("mpi_dm", class(dep_matrix))
  return(dep_matrix)

}
