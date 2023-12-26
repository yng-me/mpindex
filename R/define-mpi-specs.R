#' Define MPI specifications: dimensions, indicators, and weights
#'
#' @description Use to define MPI dimensions, indicators and its corresponding weights using any of the accessible file types: \code{.xlsx} (Excel), \code{.json}, \code{.csv}, or \code{.txt} (TSV). You can also set the poverty cutoff or list of poverty cutoffs (to achieve gradient list of MPIs) that will be used in the computation of MPI.
#'
#' @param .mpi_specs_file Accepts \code{.xlsx} (Excel), \code{.json}, \code{.csv}, or \code{.txt} (TSV) file format. This file should contain the following columns/variables: \code{Dimension}, \code{Indicator}, \code{Variable}, \code{Weight}, and \code{Description} (optional). See example below.
#' @param .indicators A data frame of MPI indicators. Useful if prefer define your indicators instead of using an external file.
#' @param .poverty_cutoffs Accepts single value or a vector of poverty cutoffs. This parameter (usually denoted by \code{k}) reflects the minimum level of deprivations or deprivation score an individual or household must be suffering simultaneously to be considered poor. See example below.
#' @param .unit_of_analysis e.g. \code{individuals}, \code{families}, \code{households}, or \code{communities}. Default value is \code{NULL}.
#' @param .aggregation Column name in the dataset that defines an aggregation level.
#' @param .uid Column name containing unique ID of the dataset which defines the lowest level of disaggregation (usually unit of analysis).
#' @param .source_of_data Source of data used in the computation. This will be used in the footnote of the table when generating an output.
#' @param .names_separator \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Column separator that defines the hierarchy of the column header.
#' @param .save_as_global_options Whether to save the specs globally. Equivalent to invoking \code{options()}.
#'
#' @return MPI specifications data frame required in \link[mpindex]{compute_mpi} function. As as side effect, a global option named `mpi_specs` will be saved for efficiency. See `getOption('mpi_specs')`.
#' @export
#'
#' @seealso \link[mpindex]{compute_mpi}
#'
#' @examples
#' # Use sample specs file included in the package
#' specs_file <- system.file(
#'   "extdata",
#'   "global-mpi-specs.csv",
#'   package = "mpindex"
#' )
#' # To see other sample specs file (with different supported file format)
#' system.file("extdata", package = "mpindex") |>
#'   list.files()
#'
define_mpi_specs <- function(
  .mpi_specs_file = NULL,
  .indicators = NULL,
  .poverty_cutoffs = 1 / 3,
  .unit_of_analysis = NULL,
  .aggregation = NULL,
  .uid = NULL,
  .source_of_data = NULL,
  .names_separator = ">",
  .save_as_global_options = TRUE
) {
  n <- NULL
  m <- NULL
  variable <- NULL
  indicator <- NULL
  dimension <- NULL


  if(!is.null(.unit_of_analysis)) {
    if (typeof(.unit_of_analysis) != "character") {
      stop(".unit_of_analysis argument only accepts string of characters.")
    }
    if (length(.unit_of_analysis) != 1) {
      stop(".unit_of_analysis argument cannot accept multiple values.")
    }
  }


  if (!is.null(.uid)) {
    .uid <- stringr::str_trim(as.character(.uid))
    if (length(.uid) != 1) {
      stop(".uid argument cannot accept multiple values.")
    }
  }

  if (length(.names_separator) != 1) {
    stop(".names_separator argument cannot accept multiple values.")
  }

  if (!(.names_separator %in% c(">", "<", "|", ".", "_", "-"))) {
    stop(".names_separator only accept the following characters: '>' (greater than), '<' (less than), '|' (pipe), '_' (underscore), '-' (dash), '.' (period).")
  }

  # accepts JSON, CSV, XLSX (Excel), TXT (TSV)
  if (!is.null(.mpi_specs_file)) {
    if (grepl("\\.xlsx$", .mpi_specs_file, ignore.case = T)) {
      df <- openxlsx::read.xlsx(.mpi_specs_file, skipEmptyRows = T, skipEmptyCols = T) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), trim_whitespace))
    } else if (grepl("\\.csv$", .mpi_specs_file, ignore.case = T)) {
      df <- utils::read.csv(.mpi_specs_file, strip.white = T)
    } else if (grepl("\\.json$", .mpi_specs_file, ignore.case = T)) {
      df <- jsonlite::read_json(.mpi_specs_file, simplifyVector = T)
    } else if (grepl("\\.txt$", .mpi_specs_file, ignore.case = T)) {
      df <- utils::read.delim(.mpi_specs_file, strip.white = T)
    } else {
      stop("Definition file format is invalid. Supports only TXT (TSV), CSV, XLSX (Excel), or JSON file format.")
    }
  } else {
    if (!is.null(.indicators)) {
      df <- .indicators
    } else {
      stop("Indictors must be defined.")
    }
  }


  df <- df |>
    clean_colnames() |>
    dplyr::select(
      dplyr::any_of(
        c("dimension", "indicator", "variable", "weight", "description")
      )
    )

  if (length(.poverty_cutoffs[.poverty_cutoffs > 1]) > 0) {
    stop(".poverty_cutoffs cannot contain values greater than 1.")
  }

  min_k <- 1 / nrow(df)
  if (length(.poverty_cutoffs[.poverty_cutoffs < min_k]) > 0) {
    stop(".poverty_cutoffs cannot contain values less than 1 divided by the total number of indicators.")
  }

  valid_colnames <- c("dimension", "indicator", "variable", "weight")
  def_colnames <- to_lowercase(sort(names(df)))

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
        to_lowercase(variable)
      ),
      label = paste0(dimension, .names_separator, indicator)
    ) |>
    dplyr::select(-c(n, m))


  attr(df, "poverty_cutoffs") <- .poverty_cutoffs
  attr(df, "unit_of_analysis") <- .unit_of_analysis
  attr(df, "aggregation") <- .aggregation
  attr(df, "uid") <- .uid
  attr(df, "source_of_data") <- .source_of_data
  attr(df, "names_separator") <- .names_separator

  class(df) <- c("mpi_specs_df", class(df))

  if (.save_as_global_options) {
    options(mpi_specs = df)
  }

  return(df)
}


validate_mpi_specs <- function(.mpi_specs) {
  if (is.null(.mpi_specs)) {
    stop("MPI specifications must be defined first.")
  }

  if (!("mpi_specs_df" %in% class(.mpi_specs))) {
    stop("Invalid MPI specifications.")
  }
}


#' Use Global MPI specification
#'
#' @description Use built-in specification file for Global MPI.
#'
#' @param ... Accepts all arguments in \code{define_mpi_specs}
#'
#' @return Global MPI specs
#' @export
#'
#' @examples
#'
#' use_global_mpi_specs()
use_global_mpi_specs <- function(...) {
  specs_file <- system.file(
    "extdata",
    "global-mpi-specs.csv",
    package = "mpindex"
  )
  define_mpi_specs(specs_file, ...)
}
