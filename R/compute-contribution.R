compute_contribution <- function(.data, ..., mpi_specs = NULL) {

  validate_mpi_specs(mpi_specs)

  w <- stats::setNames(mpi_specs$weight, mpi_specs$variable_name)
  indicators <- mpi_specs$variable_name

  contrib <- dplyr::select(.data, mpi, dplyr::all_of(indicators))
  
  contrib <- dplyr::mutate(
    contrib,
    dplyr::across(
      dplyr::all_of(indicators),
      ~ dplyr::if_else(mpi == 0, 0, (100 * w[dplyr::cur_column()] * .x) / mpi)
    )) 

  df <- dplyr::bind_cols(
    dplyr::select(.data, ..., n),
    dplyr::select(contrib, -mpi)
  )

  class(df) <- c("mpi_contribution", class(df))
  rename_indicators(df, mpi_specs = mpi_specs)

}
