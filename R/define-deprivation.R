#' Define deprivation cutoffs
#'
#' @description A deprivation cutoff must be set for each indicator defined in the MPI specifications. This step establishes the first cutoff in the methodology where every person/household (defined as the unit of analysis) can be identified as deprived or non-deprived with respect to each indicator. \cr
#'
#' For each indicator, \code{0} will be used to indicate "not deprived", \code{1} if deprived, and \code{NA} if missing or non-response. Additional column containing the product of the value of the indicator obtained and its corresponding weight will also be computed for convenience.
#'
#' @param .data A data frame or tibble
#' @param .indicator Name of indicator defined in MPI specs (must exactly match the specs).
#' @param .cutoff A conditional logic that defines the poverty line to determine whether deprived or not.
#' @param .mpi_specs MPI specifications defined in \code{\link[mpindex]{define_mpi_specs}}.
#' @param .collapse A boolean indicating whether to collapse the data frame or not. This is useful, for instance, if the original data where the \code{.cutoff} argument above applies to an individual person but your unit of analysis in household.
#' @param .set_na_equal_to Coerce value from NA to either \code{0} (not deprived) or \code{1} (deprived). Default is \code{0}.
#' @param .collapse_condition NOT YET FULLY IMPLEMENTED. ONLY WORKS WITH DEFAULT. A condition when \code{.collapse} is set to \code{TRUE}. If \code{NULL}, \code{max()} will be used as default.
#'
#' @return A data frame of deprivation value for the indicator (\code{.*_unweighted}): \code{0} for "not deprived", \code{1} for deprived, and \code{NA} for missing and non-response; and product of \code{.*_unweighted} and its corresponding weight (\code{.*_weighted}).
#'
#' @export
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/}{How to Apply the Alkire-Foster Method}
#'
#' @seealso \link[mpindex]{define_mpi_specs}
#' @examples
#' # Use sample specs file included in the package
#' specs_file <- system.file(
#'  "extdata",
#'  "global-mpi-specs.csv",
#'  package = "mpindex"
#' )
#' specs <- define_mpi_specs(specs_file, .uid = 'uuid')
#'
#' # Using built-in dataset
#' df_household |>
#'   define_deprivation(
#'     .indicator = drinking_water,
#'     .cutoff = drinking_water == 2
#'   )
#'
#' df_household_roster |>
#'   define_deprivation(
#'     .indicator = school_attendance,
#'     .cutoff = attending_school == 2,
#'     .collapse = TRUE
#'   )
#'
define_deprivation <- function(
  .data,
  .indicator,
  .cutoff,
  .mpi_specs = getOption('mpi_specs'),
  .collapse = FALSE,
  .set_na_equal_to = 0,
  .collapse_condition = NULL
) {

  validate_mpi_specs(.mpi_specs)

  variable <- NULL
  indicator <- NULL
  variable_name <- NULL
  weight <- NULL
  `:=` <- NULL
  uid <- NULL

  spec_attr <- attributes(.mpi_specs)

  selected_indicator <- .mpi_specs |>
    dplyr::filter(variable == to_enquo_str({{.indicator}}))

  if(nrow(selected_indicator) == 0) {
    stop(paste0(to_enquo_str({{.indicator}}), ' is not a valid indicator defined in the MPI specs.'))
  }

  ind <- dplyr::pull(selected_indicator, variable_name)
  v <- dplyr::pull(selected_indicator, indicator)
  w <- dplyr::pull(selected_indicator, weight)

  unweighted <- paste0(ind[1], '_unweighted')
  weighted <- paste0(ind[1], '_weighted')

  with_uid <- !is.null(spec_attr$uid)

  if(with_uid) {
    uid_name <- as.character(spec_attr$uid)
    .data <- .data |> dplyr::rename(uid = !!as.name(uid_name))
  } else {
    uid_name <- 'uid'
    .data <- .data |> tibble::rownames_to_column(var = uid_name)
  }

  if(!.set_na_equal_to %in% c(1, 0, NA)) .set_na_equal_to <- NA

  .data <- .data |>
    dplyr::transmute(
      !!as.name(uid_name) := uid,
      !!as.name(v[1]) := dplyr::if_else({{.cutoff}}, 1L, 0L, as.integer(.set_na_equal_to))
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

  class(.data) <- c("mpi_deprivation_matrix", class(.data))

  .data |>
    dplyr::mutate(!!as.name(weighted) := w[1] * !!as.name(v[1])) |>
    dplyr::rename(!!as.name(unweighted) := !!as.name(v[1]))
}
