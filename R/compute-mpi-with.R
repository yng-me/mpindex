#' Compute MPI in a single call
#'
#' @description A high-level wrapper that combines \code{\link{define_deprivation}}
#' and \code{\link{compute_mpi}} into a single call. Suitable when deprivation
#' cutoffs are defined inline rather than as intermediate objects.
#'
#' @param .data A data frame or tibble containing the primary unit-of-analysis
#'   data (e.g. household-level).
#' @param .mpi_specs MPI specifications defined in \code{\link{define_mpi_specs}}.
#' @param .deprivations A named list. Each name must match a \code{variable}
#'   column in \code{.mpi_specs}. Each element is a list with:
#'   \describe{
#'     \item{.cutoff}{(Required) A quoted expression passed to
#'       \code{\link{define_deprivation}} as the \code{.cutoff} argument.
#'       Use \code{\link[base]{quote}} or \code{\link[rlang]{expr}}.}
#'     \item{.data}{(Optional) An alternative data frame to use for this
#'       indicator instead of the global \code{.data}.}
#'     \item{.collapse}{(Optional) Boolean. Whether to collapse to the primary
#'       unit of analysis. Defaults to \code{FALSE}.}
#'     \item{.set_na_equal_to}{(Optional) Coerce NAs to 0 or 1. Defaults to 0.}
#'   }
#' @param ... Grouping columns passed to \code{\link{compute_mpi}}.
#'
#' @return An \code{mpi_output} list — same structure as
#'   \code{\link{compute_mpi}}.
#' @export
#'
#' @examples
#' \dontrun{
#' mpi_specs <- define_mpi_specs("specs.csv", .uid = "uuid")
#'
#' compute_mpi_with(
#'   .data = df_household,
#'   .mpi_specs = mpi_specs,
#'   .deprivations = list(
#'     drinking_water = list(.cutoff = quote(drinking_water == 2)),
#'     school_attendance = list(
#'       .data = df_household_roster,
#'       .cutoff = quote(attending_school == 2),
#'       .collapse = TRUE
#'     )
#'   )
#' )
#' }
#'
compute_mpi_with <- function(
  .data,
  .mpi_specs = getOption("mpi_specs"),
  .deprivations,
  ...
) {

  validate_mpi_specs(.mpi_specs)

  indicator_names <- names(.deprivations)
  deprivation_profile <- vector("list", length(indicator_names))
  names(deprivation_profile) <- indicator_names

  for (ind in indicator_names) {
    entry <- .deprivations[[ind]]

    data_for_ind <- if (!is.null(entry$.data)) entry$.data else .data
    cutoff_expr <- entry$.cutoff
    collapse <- if (!is.null(entry$.collapse)) entry$.collapse else FALSE
    set_na <- if (!is.null(entry$.set_na_equal_to)) entry$.set_na_equal_to else 0

    deprivation_profile[[ind]] <- rlang::inject(
      define_deprivation(
        .data = data_for_ind,
        .indicator = !!rlang::sym(ind),
        .cutoff = !!cutoff_expr,
        .mpi_specs = .mpi_specs,
        .collapse = collapse,
        .set_na_equal_to = set_na
      )
    )
  }

  .data |> compute_mpi(deprivation_profile, .mpi_specs = .mpi_specs, ...)
}
