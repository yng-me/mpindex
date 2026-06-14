#' Specify a deprivation cutoff for use in \code{compute_mpi}
#'
#' @description A helper that captures a bare deprivation cutoff expression and
#'   optional per-indicator settings for use inside the \code{deprivations}
#'   argument of \code{\link{compute_mpi}}.
#'
#' @param .cutoff A bare logical expression evaluated against the indicator's
#'   data frame. Rows where this evaluates to \code{TRUE} are considered
#'   deprived.
#' @param .data An optional data frame to use for this indicator instead of the
#'   primary \code{.data} passed to \code{compute_mpi}. Useful when one or more
#'   indicators are at a different unit of analysis (e.g. person-level roster).
#' @param collapse_fn An optional function applied to collapse roster-level
#'   data to the unit-of-analysis level (e.g. \code{max} to flag a household as
#'   deprived if any member is deprived). If \code{NULL} (default), no
#'   collapsing is performed. NAs are removed before calling the function; if
#'   all values are \code{NA} the result is \code{NA}.
#' @param set_na_equal_to Coerce \code{NA} deprivation values to \code{0}
#'   (not deprived, default) or \code{1} (deprived).
#' @param ... Reserved; passing old dotted names (e.g. \code{.collapse_fn})
#'   triggers a helpful error.
#'
#' @return An object of class \code{mpi_d}.
#' @export
#'
#' @seealso \link[mpindex]{compute_mpi}
#'
#' @examples
#' deprived(drinking_water == 2)
#' deprived(undernourished == 1 & age < 70, .data = df_household_roster, collapse_fn = max)
#'
deprived <- function(
  .cutoff,
  .data          = NULL,
  collapse_fn    = NULL,
  set_na_equal_to = 0,
  ...
) {
  check_old_dotted_args("deprived", c(".collapse_fn", ".set_na_equal_to"), ...)
  structure(
    list(
      cutoff          = rlang::enquo(.cutoff),
      data            = .data,
      collapse_fn     = collapse_fn,
      set_na_equal_to = set_na_equal_to
    ),
    class = "mpi_d"
  )
}


#' Define deprivation cutoffs
#'
#' @description Sets a deprivation cutoff for a single indicator. For each
#'   unit of analysis, the result is \code{0} (not deprived), \code{1}
#'   (deprived), or \code{NA} (missing). An additional weighted column
#'   (indicator value × weight) is also computed.
#'
#' @param .data A data frame or tibble.
#' @param indicator Name of the indicator as defined in the MPI Specs (must
#'   exactly match the \code{variable} column).
#' @param cutoff A logical expression that evaluates to \code{TRUE} for
#'   deprived units.
#' @param mpi_specs MPI specifications from \code{\link{define_mpi_specs}}.
#' @param collapse_fn An optional function to collapse roster-level data to
#'   the unit-of-analysis level (e.g. \code{max}). \code{NULL} (default)
#'   means no collapsing.
#' @param set_na_equal_to Coerce \code{NA} values to \code{0} (not deprived,
#'   default) or \code{1} (deprived).
#' @param ... Reserved; passing old dotted names triggers a helpful error.
#'
#' @return A data frame with columns \code{*_unweighted} and \code{*_weighted}.
#'
#' @export
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/}{How to Apply the Alkire-Foster Method}
#'
#' @seealso \link[mpindex]{define_mpi_specs}
#' @examples
#' specs_file <- system.file(
#'  "extdata",
#'  "global-mpi-specs.csv",
#'  package = "mpindex"
#' )
#'
#' specs <- define_mpi_specs(specs_file, uid = "uuid")
#'
#' df_household |>
#'   define_deprivation(
#'     indicator  = drinking_water,
#'     cutoff     = drinking_water == 2,
#'     mpi_specs  = specs
#'   )
#'
#' df_household_roster |>
#'   define_deprivation(
#'     indicator   = school_attendance,
#'     cutoff      = attending_school == 2,
#'     mpi_specs   = specs,
#'     collapse_fn = max
#'   )
#'
define_deprivation <- function(
  .data,
  indicator,
  cutoff,
  mpi_specs       = NULL,
  collapse_fn     = NULL,
  set_na_equal_to = 0,
  ...
) {
  check_old_dotted_args(
    "define_deprivation",
    c(".indicator", ".cutoff", ".mpi_specs", ".collapse_fn",
      ".collapse", ".collapse_condition", ".set_na_equal_to"),
    ...
  )

  validate_mpi_specs(mpi_specs)
  spec_attr <- attributes(mpi_specs)

  selected_indicator <- dplyr::filter(mpi_specs, variable == to_enquo_str({{indicator}}))

  if (nrow(selected_indicator) == 0) {
    stop(paste0(to_enquo_str({{indicator}}), " is not a valid indicator defined in the MPI specs."))
  }

  ind <- dplyr::pull(selected_indicator, variable_name)
  v   <- dplyr::pull(selected_indicator, indicator)
  w   <- dplyr::pull(selected_indicator, weight)

  unweighted <- paste0(ind[1], "_unweighted")
  weighted   <- paste0(ind[1], "_weighted")

  with_uid <- !is.null(spec_attr$uid)

  if (with_uid) {
    uid_name <- as.character(spec_attr$uid)
    .data <- dplyr::rename(.data, uid = !!as.name(uid_name))
  } else {
    uid_name <- "uid"
    .data <- tibble::rownames_to_column(.data, var = uid_name)
  }

  if (!set_na_equal_to %in% c(1, 0, NA)) set_na_equal_to <- NA

  .data <- dplyr::transmute(
    .data,
    !!as.name(uid_name) := uid,
    !!as.name(v[1])     := dplyr::if_else({{cutoff}}, 1L, 0L, as.integer(set_na_equal_to))
  )

  if (!is.null(collapse_fn) && with_uid) {
    collapse_safe <- function(x) {
      x_clean <- x[!is.na(x)]
      if (length(x_clean) == 0L) return(NA_integer_)
      result <- collapse_fn(x_clean)
      if (length(result) != 1L) stop("collapse_fn must return a scalar value.")
      as.integer(result)
    }
    .data <- .data |>
      dplyr::group_by(!!as.name(uid_name)) |>
      dplyr::summarise(
        !!as.name(v[1]) := collapse_safe(!!as.name(v[1])),
        .groups = "drop"
      )
  }

  class(.data) <- c("mpi_dm", class(.data))

  .data |>
    dplyr::mutate(!!as.name(weighted) := w[1] * !!as.name(v[1])) |>
    dplyr::rename(!!as.name(unweighted) := !!as.name(v[1]))
}
