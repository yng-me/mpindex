compute_contribution <- function(
    .data,
    ...,
    .mpi_specs = getOption("mpi_specs")) {
  validate_mpi_specs(.mpi_specs)

  n <- NULL
  MPI <- NULL
  `:=` <- NULL
  spec_attr <- attributes(.mpi_specs)

  df_contrib <- .data |>
    dplyr::select(dplyr::any_of(attributes(spec_attr$aggregation)), n, ...)

  w <- .mpi_specs$weight
  indicator <- .mpi_specs$variable_name

  for (i in seq_along(indicator)) {
    contrib <- .data |>
      dplyr::select(MPI, !!as.name(indicator[i])) |>
      dplyr::transmute(
        !!as.name(indicator[i]) := dplyr::if_else(
          MPI == 0,
          0,
          (100 * (w[i] * !!as.name(indicator[i]))) / MPI
        )
      )

    df_contrib <- df_contrib |>
      dplyr::bind_cols(contrib)
  }

  return(df_contrib |> rename_indicators(.mpi_specs = .mpi_specs))
}
