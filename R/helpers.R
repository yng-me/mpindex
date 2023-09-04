bind_list <- function(.data, .list, .by) {

  list_names <- names(.list)

  for(i in seq_along(list_names)) {
    .data <- .data |>
      dplyr::left_join(.list[[list_names[i]]], by = .by)
  }
  return(.data)
}

rename_indicators <- function(.data, .mpi_specs = getOption('mpi_specs')) {

  value <- NULL
  label <- NULL

  .mpi_colnames <- names(.data) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable_name = stringr::str_remove(value, '_(uncensored_k|uncensored|censored|)$')) |>
    dplyr::left_join(.mpi_specs$indicators, by = 'variable_name') |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
    dplyr::pull(label)

  colnames(.data) <- .mpi_colnames
  return(.data)
}


to_enquo_str <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}
