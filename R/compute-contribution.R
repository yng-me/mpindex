compute_contribution <- function(
  .data,
  ...,
  .mpi_specs = getOption("mpi_specs")

) {

  validate_mpi_specs(.mpi_specs)

  n <- NULL
  mpi <- NULL
  `:=` <- NULL
  spec_attr <- attributes(.mpi_specs)

  df <- .data |>
    dplyr::select(dplyr::any_of(spec_attr$aggregation), n, ...)

  w <- .mpi_specs$weight
  indicator <- .mpi_specs$variable_name

  for (i in seq_along(indicator)) {
    contrib <- .data |>
      dplyr::select(mpi, !!as.name(indicator[i])) |>
      dplyr::transmute(
        !!as.name(indicator[i]) := dplyr::if_else(
          mpi == 0,
          0,
          (100 * (w[i] * !!as.name(indicator[i]))) / mpi
        )
      )

    df <- df |>
      dplyr::bind_cols(contrib)
  }

  class(df) <- c("mpi_contribution", class(df))

  return(df |> rename_indicators(.mpi_specs = .mpi_specs))
}
