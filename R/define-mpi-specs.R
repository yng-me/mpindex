#' Define MPI specifications: dimensions, indicators, and weights
#'
#' @description Use to define MPI dimensions, indicators and its corresponding
#'   weights using any of the supported file types: \code{.xlsx} (Excel),
#'   \code{.json}, \code{.csv}, or \code{.txt} (TSV). You can also set the
#'   poverty cutoff or list of poverty cutoffs.
#'
#' @param mpi_specs_file Path to a \code{.xlsx}, \code{.json}, \code{.csv}, or
#'   \code{.txt} (TSV) file. The file must contain columns: \code{Dimension},
#'   \code{Indicator}, \code{Variable}, \code{Weight}, and optionally
#'   \code{Description}.
#' @param indicators A data frame of MPI indicators. Alternative to
#'   \code{mpi_specs_file} when you prefer to define indicators inline.
#' @param poverty_cutoffs Single value or vector of poverty cutoffs (k).
#'   All values must be in (0, 1]. Default is \code{NULL} which will be automatically set to \code{1/n}, where \code{n} is the total number of dimensions.
#' @param unit_of_analysis e.g. \code{"individuals"}, \code{"households"}.
#'   Default \code{NULL}.
#' @param uid Column name containing the unique ID (unit of analysis).
#' @param source_of_data Source of data; used in output footnotes.
#' @param names_separator \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Column separator for the header hierarchy.
#' @param save_as_global_options \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} No longer has any effect.
#' @param ... Reserved for forward-compatibility; passing old dotted argument
#'   names (e.g. \code{.uid}) triggers a helpful error.
#'
#' @return An \code{mpi_specs} object. Pass this directly as the
#'   \code{mpi_specs} argument in \code{\link{compute_mpi}},
#'   \code{\link{define_deprivation}}, and \code{\link{save_mpi}}.
#' @export
#'
#' @seealso \link[mpindex]{compute_mpi}
#'
#' @examples
#' specs_file <- system.file(
#'   "extdata",
#'   "global-mpi-specs.csv",
#'   package = "mpindex"
#' )
#' system.file("extdata", package = "mpindex") |> list.files()
#'
define_mpi_specs <- function(
  mpi_specs_file    = NULL,
  indicators        = NULL,
  poverty_cutoffs   = NULL,
  unit_of_analysis  = NULL,
  uid               = NULL,
  source_of_data    = NULL,
  names_separator   = getOption('mpindex.options')$names_separator %||% "__",
  save_as_global_options = FALSE,
  ...
) {
  dots <- rlang::list2(...)
  if (any(c("aggregation", ".aggregation") %in% names(dots))) {
    rlang::abort(
      paste0(
        "The `aggregation` argument has been removed from `define_mpi_specs()`. ",
        "Pass grouping columns via `by` in `compute_mpi()` instead."
      ),
      call = rlang::caller_env()
    )
  }

  check_old_dotted_args(
    "define_mpi_specs",
    c(".mpi_specs_file", ".indicators", ".poverty_cutoffs", ".unit_of_analysis",
      ".uid", ".source_of_data", ".names_separator",
      ".save_as_global_options"),
    ...
  )

  if (!is.null(unit_of_analysis)) {
    if (typeof(unit_of_analysis) != "character") {
      stop("unit_of_analysis argument only accepts string of characters.")
    }
    if (length(unit_of_analysis) != 1) {
      stop("unit_of_analysis argument cannot accept multiple values.")
    }
  }

  if (!is.null(uid)) {
    uid <- stringr::str_trim(as.character(uid))
    if (length(uid) != 1) {
      stop("uid argument cannot accept multiple values.")
    }
  }

  if (length(names_separator) != 1) {
    stop("names_separator argument cannot accept multiple values.")
  }

  # accepts JSON, CSV, XLSX (Excel), TXT (TSV)
  if (!is.null(mpi_specs_file)) {
    if (grepl("\\.xlsx$", mpi_specs_file, ignore.case = TRUE)) {
      df <- openxlsx::read.xlsx(mpi_specs_file, skipEmptyRows = TRUE, skipEmptyCols = TRUE) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))
    } else if (grepl("\\.csv$", mpi_specs_file, ignore.case = TRUE)) {
      df <- utils::read.csv(mpi_specs_file, strip.white = TRUE)
    } else if (grepl("\\.json$", mpi_specs_file, ignore.case = TRUE)) {
      df <- jsonlite::read_json(mpi_specs_file, simplifyVector = TRUE)
    } else if (grepl("\\.txt$", mpi_specs_file, ignore.case = TRUE)) {
      df <- utils::read.delim(mpi_specs_file, strip.white = TRUE)
    } else {
      stop("Definition file format is invalid. Supports only TXT (TSV), CSV, XLSX (Excel), or JSON file format.")
    }
  } else {
    if (!is.null(indicators)) {
      df <- indicators
    } else {
      stop("Indicators must be defined.")
    }
  }

  df <- df |>
    clean_colnames() |>
    dplyr::select(
      dplyr::any_of(c("dimension", "indicator", "variable", "weight", "description"))
    )

  if(is.null(poverty_cutoffs)) {
    poverty_cutoffs <- 1 / length(unique(df$dimension))
  }

  if (length(poverty_cutoffs[poverty_cutoffs > 1]) > 0) {
    stop("poverty_cutoffs cannot contain values greater than 1.")
  }

  min_k <- 1 / nrow(df)
  if (length(poverty_cutoffs[poverty_cutoffs < min_k]) > 0) {
    stop("poverty_cutoffs cannot contain values less than 1 divided by the total number of indicators.")
  }

  k_labels <- set_k_label(poverty_cutoffs)
  if (length(unique(k_labels)) != length(k_labels)) {
    stop("poverty_cutoffs must produce unique labels when rounded to the nearest percent (e.g. 1/3 and 0.334 both round to k_33).")
  }

  valid_colnames <- c("dimension", "indicator", "variable", "weight")
  def_colnames   <- tolower(sort(names(df)))

  is_colnames_identical <- identical(def_colnames, valid_colnames) |
    identical(def_colnames, c("description", valid_colnames))

  if (!is_colnames_identical) stop("Invalid column names found.")

  dimensions <- df |>
    dplyr::distinct(dimension) |>
    dplyr::mutate(m = seq_along(dimension))

  df <- df |>
    dplyr::group_by(dimension) |>
    dplyr::mutate(n = seq_along(dimension)) |>
    dplyr::ungroup() |>
    dplyr::left_join(dimensions, by = "dimension") |>
    dplyr::mutate(
      variable_name = paste0(
        "d",
        stringr::str_pad(m, width = 2, pad = "0"),
        "_i",
        stringr::str_pad(n, width = 2, pad = "0"),
        "_",
        tolower(variable)
      ),
      label = paste0(dimension, names_separator, indicator)
    ) |>
    dplyr::select(-c(n, m))

  attr(df, "poverty_cutoffs")  <- poverty_cutoffs
  attr(df, "unit_of_analysis") <- unit_of_analysis
  attr(df, "uid")              <- uid
  attr(df, "source_of_data")   <- source_of_data
  attr(df, "names_separator")  <- names_separator

  class(df) <- c("mpi_specs", class(df))

  if (!missing(save_as_global_options) && isTRUE(save_as_global_options)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "define_mpi_specs(save_as_global_options)",
      details = "Pass the returned object directly as `mpi_specs` in downstream functions."
    )
  }

  return(df)
}


validate_mpi_specs <- function(mpi_specs) {
  if (is.null(mpi_specs)) {
    rlang::abort(
      paste0(
        "No MPI Specs supplied. ",
        "Pass the result of `define_mpi_specs()` as `mpi_specs`, ",
        "or use `global_mpi_specs()` to load the built-in Global MPI spec."
      ),
      call = rlang::caller_env()
    )
  }

  if (!("mpi_specs" %in% class(mpi_specs))) {
    rlang::abort(
      "`mpi_specs` must be an `mpi_specs` object returned by `define_mpi_specs()`.",
      call = rlang::caller_env()
    )
  }
}


#' Load the built-in Global MPI specification
#'
#' @description Returns the \code{mpi_specs} object for the standard
#'   Global MPI (10 indicators across Health, Education, and Living Standards).
#'   Assign the result and pass it explicitly as \code{mpi_specs}.
#'
#' @param ... Additional arguments passed to \code{\link{define_mpi_specs}},
#'   e.g. \code{uid}, \code{poverty_cutoffs}.
#' @param poverty_cutoffs Single value or vector of poverty cutoffs (k).
#'   All values must be in (0, 1]. Default is \code{1/3}.
#'
#' @return An \code{mpi_specs} object.
#' @export
#'
#' @examples
#' mpi_specs <- global_mpi_specs(uid = "uuid")
#'
global_mpi_specs <- function(..., poverty_cutoffs = 1/3) {
  specs_file <- system.file(
    "extdata",
    "global-mpi-specs.csv",
    package = "mpindex"
  )
  define_mpi_specs(specs_file, poverty_cutoffs = poverty_cutoffs, ...)
}


#' Use Global MPI specification
#'
#' @description \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
#'   Please use \code{\link{global_mpi_specs}} instead.
#'
#' @param ... Passed to \code{\link{global_mpi_specs}}.
#'
#' @return An \code{mpi_specs} object.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated — use global_mpi_specs() instead
#' mpi_specs <- use_global_mpi_specs(uid = "uuid")
#' }
#'
use_global_mpi_specs <- function(...) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "use_global_mpi_specs()",
    "global_mpi_specs()"
  )
  global_mpi_specs(...)
}

