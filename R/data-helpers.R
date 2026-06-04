bind_list <- function(.data, .list, .by) {
  Reduce(
    function(acc, y) dplyr::left_join(acc, y, by = .by, multiple = "first"),
    .list,
    init = .data
  )
}

rename_indicators <- function(.data, .mpi_specs = getOption("mpi_specs")) {

  mpi_colnames <- names(.data) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable_name = value) |>
    dplyr::left_join(.mpi_specs, by = "variable_name") |>
    dplyr::mutate(label = dplyr::if_else(
      is.na(label),
      to_title_case(value),
      indicator
    ))

  for(i in seq_along(mpi_colnames$value)) {
    mpi_col <- mpi_colnames$value[i]
    attr(.data[[mpi_col]], "label") <- mpi_colnames$label[i]

    if(!is.na(mpi_colnames$dimension[i])) {
      attr(.data[[mpi_col]], "dimension") <- mpi_colnames$dimension[i]
    }

    if(!is.na(mpi_colnames$indicator[i])) {
      attr(.data[[mpi_col]], "indicator") <- mpi_colnames$indicator[i]
    }
  }

  if ("n" %in% names(.data)) {
    attr_specs <- attributes(.mpi_specs)
    .data <- .data |>
      rename_n(attr_specs$unit_of_analysis)
  }

  if ("uuid" %in% names(.data)) {
    attr(.data$uuid, "label") <- "UUID"
  }

  if ("deprivation_score" %in% names(.data)) {
    attr(.data$deprivation_score, "label") <- "Deprivation score"
  }

  .data <- .data |>
    dplyr::rename_with(~ stringr::str_remove_all(., '^d\\d{2}_i\\d{2}_'))

  return(.data)
}

rename_n <- function(.data, .label) {

  if(is.null(.label)) .label <- "cases"

  mpi_colname <- paste0("number_of_", tolower(.label))

  .data <- .data |>
    dplyr::rename(!!as.name(mpi_colname) := n)

  attr(.data[[mpi_colname]], "label") <- paste0("Number of ", .label)

  return(.data)
}

to_enquo_str <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), "~")
}

set_cutoff_label <- function(.value) {
  paste0(round(.value * 100), "% poverty cutoff")
}

set_k_label <- function(.value) {
  paste0("k_", round(.value * 100))
}

set_dep_label <- function(.value, .index) {
  if (length(.value) == 1) {
    .v <- "censored"
  } else if (length(.value) > 1) {
    .v <- .value[.index]
  } else {
    .v <- "na"
  }
  return(.v)
}
