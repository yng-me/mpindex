compute_contribution <- function(
  .data,
  ...,
  mpi_specs = NULL
) {

  validate_mpi_specs(mpi_specs)
  spec_attr <- attributes(mpi_specs)

  w          <- stats::setNames(mpi_specs$weight, mpi_specs$variable_name)
  indicators <- mpi_specs$variable_name

  contrib <- .data |>
    dplyr::select(mpi, dplyr::all_of(indicators)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(indicators),
      ~ dplyr::if_else(mpi == 0, 0, (100 * w[dplyr::cur_column()] * .x) / mpi)
    )) |>
    dplyr::select(-mpi)

  df <- dplyr::bind_cols(
    dplyr::select(.data, dplyr::any_of(spec_attr$aggregation), n, ...),
    contrib
  )

  class(df) <- c("mpi_contribution", class(df))

  rename_indicators(df, mpi_specs = mpi_specs)
}
