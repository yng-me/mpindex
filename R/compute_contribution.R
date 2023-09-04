compute_contribution <- function(
  .data,
  ...,
  .mpi_specs = getOption('mpi_specs')
) {

  n <- NULL
  M_0 <- NULL
  `:=` <- NULL

  .df_contrib <- .data |> dplyr::select(..., n)
  .w <- .mpi_specs$indicators$weight
  .mpi_label <- .mpi_specs$indicators$label

  for(i in seq_along(.mpi_label)) {

    .contrib <- .data |>
      dplyr::select(M_0, !!as.name(.mpi_label[i])) |>
      dplyr::transmute(
        !!as.name(paste0(.mpi_label[i])) := dplyr::if_else(
          M_0 == 0,
          0,
          100 * (.w[i] * !!as.name(.mpi_label[i])) / M_0
        )
      )

    .df_contrib <- .df_contrib |>
      dplyr::bind_cols(.contrib)
  }

  return(.df_contrib)
}
