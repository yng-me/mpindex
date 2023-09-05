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
  deprivation_score <- NULL

  .mpi_colnames <- names(.data) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable_name = value) |>
    dplyr::left_join(.mpi_specs$indicators, by = 'variable_name') |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
    dplyr::pull(label)

  colnames(.data) <- .mpi_colnames

  if('n' %in% names(.data)) {
    .data <- .data |>
      rename_n(.mpi_specs$unit_of_analysis)
  }

  if('deprivation_score' %in% names(.data)) {
    .data <- .data |>
      dplyr::rename('Deprivation score' = deprivation_score)
  }

  return(.data)
}

rename_n <- function(.data, .label) {
  n <- NULL
  `:=` <- NULL
  .data |>
    dplyr::rename(!!as.name(paste0('Number of ', .label)) := n)
}

to_enquo_str <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}

set_cutoff_label <- function(.value) {
  paste0(round(.value * 100), '% poverty cutoff')
}

set_dep_label <- function(.value, .index) {
  if(length(.value) == 1) {
    .v <- 'Censored'
  } else if (length(.value) > 1) {
    .v <- .value[.index]
  } else {
    v <- 'Not applicable'
  }
  return(.v)
}

