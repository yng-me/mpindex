compute_contribution <- function(
  .data,
  ...,
  .mpi_specs = getOption('mpi_specs')
) {

  n <- NULL
  MPI <- NULL
  `:=` <- NULL

  .df_contrib <- .data |> dplyr::select(dplyr::any_of(.mpi_specs$aggregation), n, ...)

  .w <- .mpi_specs$indicators$weight
  .ind <- .mpi_specs$indicators$variable_name

  for(i in seq_along(.ind)) {

    .contrib <- .data |>
      dplyr::select(MPI, !!as.name(.ind[i])) |>
      dplyr::transmute(
        !!as.name(.ind[i]) := dplyr::if_else(
          MPI == 0,
          0,
          (100 * (.w[i] * !!as.name(.ind[i]))) / MPI
        )
      )

    .df_contrib <- .df_contrib |>
      dplyr::bind_cols(.contrib)
  }

  return(.df_contrib |> rename_indicators(.mpi_specs = .mpi_specs))
}
