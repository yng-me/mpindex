#' Define MPI specifications: dimensions, indicators, and weights
#'
#' @description Use to define MPI dimensions, indicators and its corresponding weights using any of the accessible file types: \code{.xlsx} (Excel), \code{.json}, \code{.csv}, or \code{.txt} (TSV). You can also set the poverty cutoff or list of poverty cutoffs (to achieve gradient list of MPIs) that will be used in the computation of MPI.
#'
#' @param .mpi_specs_file Accepts \code{.xlsx} (Excel), \code{.json}, \code{.csv}, or \code{.txt} (TSV) file format. This file should contain the following columns/variables: \code{Dimension}, \code{Indicator}, \code{Variable}, \code{Weight}, and \code{Description} (optional). See example below.
#' @param .poverty_cutoffs Accepts single value or a vector of poverty cutoffs. See example below.
#' @param .unit_of_analysis e.g. \code{individuals}, \code{families}, \code{households}, or \code{communities}. Default value is \code{households}.
#' @param .uid Column name containing unique ID of the observation which defines the lowest level of disaggregation (usually unit of analysis).
#' @param .source_of_data Source of data used in the computation. This will be used in the footnote of the table when generating an output.
#' @param .names_separator Column separator that defines the hierarchy of the column header.
#'
#' @return A list of objects containing MPI specifications needed by \link[mpindex]{compute_mpi} function.
#' @export
#'
#' @seealso \link[mpindex]{compute_mpi}
#'
#' @examples
#' # Use sample specs file included in the package
#' specs_file <- system.file(
#'  "extdata",
#'  "mpi-specs-sample.csv",
#'  package = "mpindex"
#' )
#' # To see other sample specs file (with different supported file format)
#' system.file("extdata", package = "mpindex") |>
#'   list.files()
#'
#' # OPTIONS:
#' # 1. Pass this `specs` object to `compute_mpi` function
#' #
#' specs <- define_mpi_specs(specs_file)
#'
#' # 2. Make it available globally (recommended approach)
#' options(mpi_specs = specs)
#'

define_mpi_specs <- function(
  .mpi_specs_file,
  .poverty_cutoffs = 1/3,
  .unit_of_analysis = 'households',
  .uid = NULL,
  .source_of_data = NULL,
  .names_separator = '>'
) {

  n <- NULL
  m <- NULL
  value <- NULL
  label <- NULL
  variable <- NULL
  indicator <- NULL
  dimension <- NULL

  # accepts JSON, CSV, XLSX (Excel), TXT (TSV)
  if(grepl('\\.xlsx$', .mpi_specs_file, ignore.case = T)) {

    df <- openxlsx::read.xlsx(.mpi_specs_file, skipEmptyRows = T, skipEmptyCols = T) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character), trim_whitespace))

  } else if(grepl('\\.csv$', .mpi_specs_file, ignore.case = T)) {

    df <- utils::read.csv(.mpi_specs_file, strip.white = T)

  } else if(grepl('\\.json$', .mpi_specs_file, ignore.case = T)) {

    df <- jsonlite::read_json(.mpi_specs_file, simplifyVector = T)

  } else if(grepl('\\.txt$', .mpi_specs_file, ignore.case = T)) {

    df <- utils::read.delim(.mpi_specs_file, quote = '', strip.white = T)

  } else {

    stop('Definition file format is invalid. Supports only TXT (TSV), CSV, XLSX (Excel), or JSON file format.')

  }

  valid_colnames <- c("dimension", "indicator", "variable", "weight")
  def_colnames <- to_lowercase(sort(names(df)))

  is_colnames_identical <- identical(def_colnames, valid_colnames) |
    identical(def_colnames, c('description', valid_colnames))

  if(!is_colnames_identical) stop('Invalid column names found.')

  dimensions <- df |>
    clean_colnames() |>
    dplyr::distinct(dimension) |>
    dplyr::mutate(m = seq_along(dimension))

  df <- df |>
    clean_colnames() |>
    dplyr::group_by(dimension) |>
    dplyr::mutate(n = seq_along(dimension)) |>
    dplyr::ungroup() |>
    dplyr::left_join(dimensions, by = 'dimension') |>
    dplyr::mutate(
      variable_name = paste0(
        'd',
        stringr::str_pad(m, width = 2, pad = '0'),
        '_i',
        stringr::str_pad(n, width = 2, pad = '0'),
        '_',
        to_lowercase(variable)
      ),
      label = paste0(dimension, .names_separator, indicator)
    ) |>
    dplyr::select(-c(n, m))

  return(list(
    indicators = df,
    poverty_cutoffs = .poverty_cutoffs,
    uid = .uid,
    unit_of_analysis = .unit_of_analysis,
    source_of_data = .source_of_data
  ))
}
