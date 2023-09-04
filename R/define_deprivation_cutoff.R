#' Define deprivation cutoffs
#'
#' @param .data A data frame or tibble
#' @param .indicator A string or symbol indicating the name of indicator to be used
#' @param .condition A string or symbol indicating the condition to be used
#' @param .mpi_specs A data frame or tibble containing specifications of the MPI
#' @param .collapse A boolean indicating whether to collapse the data frame or not
#' @param .collapse_condition A string or symbol indicating the condition to be used when collapsing the data
#'
#' @return
#' @export
#'
#' @examples
#'

define_deprivation_cutoff <- function(
  .data,
  .indicator,
  .condition,
  .mpi_specs = getOption('mpi_specs'),
  .collapse = FALSE,
  .collapse_condition = NULL
) {

  variable <- NULL
  indicator <- NULL
  variable_name <- NULL
  weight <- NULL
  `:=` <- NULL
  row_id <- NULL


  if(is.null(.mpi_specs)) {
    stop('MPI specifications must be defined first.')
  }

  selected_indicator <- .mpi_specs$indicators |>
    dplyr::filter(variable == to_enquo_str({{.indicator}}))

  if(nrow(selected_indicator) == 0) {
    stop(paste0(to_enquo_str({{.indicator}}), ' is not a valid indicator defined in the MPI specs.'))
  }

  ind <- dplyr::pull(selected_indicator, variable_name)
  v <- dplyr::pull(selected_indicator, indicator)
  w <- dplyr::pull(selected_indicator, weight)

  uncensored <- paste0(ind[1], '_uncensored')
  censored <- paste0(ind[1], '_censored')

  with_row_id <- !is.null(.mpi_specs$row_id)

  if(with_row_id) {
    row_id_name <- as.character(.mpi_specs$row_id)
    .data <- .data |> dplyr::rename(row_id = !!as.name(row_id_name))
  } else {
    row_id_name <- 'row_id'
    .data <- .data |> tibble::rownames_to_column(var = row_id_name)
  }

  .data <- .data |>
    dplyr::transmute(
      !!as.name(row_id_name) := row_id,
      !!as.name(v[1]) := dplyr::if_else({{.condition}}, 1L, 0L, NA_integer_)
    )

  if(.collapse & with_row_id) {
    .data <- .data |>
      dplyr::group_by(!!as.name(row_id_name))

    if(to_enquo_str({{.collapse_condition}}) != 'NULL') {
      .data <- .data |>
        dplyr::summarise(
          !!as.name(v[1]) := dplyr::if_else(
            {{.collapse_condition}}, 1L, 0L, NA_integer_
          ),
          .groups = 'drop'
        )

    } else {
      .data <- .data |>
        dplyr::summarise(
          !!as.name(v[1]) := max(!!as.name(v[1]), na.rm = T),
          .groups = 'drop'
        )
    }
  }

  .data |>
    dplyr::mutate(!!as.name(censored) := w[1] * !!as.name(v[1])) |>
    dplyr::rename(!!as.name(uncensored) := !!as.name(v[1]))
}
