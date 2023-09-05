#' Define deprivation cutoffs
#'
#' @description A deprivation cutoff must be set for each indicator defined in the MPI specifications. This step establishes the first cutoff in the methodology where every person/household (defined as the unit of analysis) can be identified as deprived or non-deprived with respect to each indicator. \cr
#'
#' For each indicator, \code{0} will be used to indicate "not deprived", \code{1} if deprived, and \code{NA} if missing or non-response. Additional column containing the product of the value of the indicator obtained and its corresponding weight will also be computed for convenience.
#'
#' @param .data A data frame or tibble
#' @param .indicator Name of indicator defined in MPI specs (must exactly match the specs).
#' @param .condition A conditional logic that defines the poverty line to determine whether deprived or not.
#' @param .mpi_specs MPI specifications defined from \code{\link[mpindex]{define_mpi_specs}}.
#' @param .collapse A boolean indicating whether to collapse the data frame or not. This is useful, for instance, if the original data where the \code{.condition} argument above applies to an individual person but your unit of analysis in household.
#' @param .collapse_condition NOT YET FULLY IMPLEMENTED. ONLY WORKS WITH DEFAULT. A condition when \code{.collapse} is set to \code{TRUE}. If \code{NULL}, \code{max()} will be used as default.
#'
#' @return A data frame of deprivation value for the indicator (\code{.*_unweighted}): \code{0} for "not deprived", \code{1} for deprived, and \code{NA} for missing and non-response; and product of \code{.*_unweighted} and its corresponding weight (\code{.*_weighted}).
#'
#' @export
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/}{How to Apply the Alkire-Foster Method}
#'
#' @seealso \link[mpindex]{define_mpi_specs}
#' @examples
#' #TODO

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
  uid <- NULL


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

  unweighted <- paste0(ind[1], '_unweighted')
  weighted <- paste0(ind[1], '_weighted')

  with_uid <- !is.null(.mpi_specs$uid)

  if(with_uid) {
    uid_name <- as.character(.mpi_specs$uid)
    .data <- .data |> dplyr::rename(uid = !!as.name(uid_name))
  } else {
    uid_name <- 'uid'
    .data <- .data |> tibble::rownames_to_column(var = uid_name)
  }

  .data <- .data |>
    dplyr::transmute(
      !!as.name(uid_name) := uid,
      !!as.name(v[1]) := dplyr::if_else({{.condition}}, 1L, 0L, NA_integer_)
    )

  if(.collapse & with_uid) {
    .data <- .data |>
      dplyr::group_by(!!as.name(uid_name))

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
    dplyr::mutate(!!as.name(weighted) := w[1] * !!as.name(v[1])) |>
    dplyr::rename(!!as.name(unweighted) := !!as.name(v[1]))
}
