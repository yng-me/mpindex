#' Save MPI output
#'
#' @description Save the MPI output to an Excel file using the
#' \href{https://cran.r-project.org/package=tsg}{tsg} package for
#' publication-ready table formatting.
#'
#' @param mpi_output An object derived from \link[mpindex]{compute_mpi}.
#' @param mpi_specs MPI specifications defined in
#'   \code{\link[mpindex]{define_mpi_specs}}.
#' @param filename Output filename. The \code{.xlsx} extension is added
#'   automatically when missing. Defaults to \code{"MPI Results.xlsx"} in the
#'   current working directory.
#' @param include_deprivation_matrix Whether to include deprivation matrices
#'   as separate sheets. Defaults to \code{TRUE}.
#' @param include_specs Whether to include MPI specification as a separate
#'   sheet. Defaults to \code{FALSE}.
#' @param ... Reserved; passing old dotted names triggers a helpful error.
#'
#' @return Returns the normalised file path of the generated Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#' mpi_result <- compute_mpi(df_household, mpi_specs = specs, deprivations = deps)
#' save_mpi(mpi_result, mpi_specs = specs, filename = "MPI Sample Output")
#' }
#'
save_mpi <- function(
  mpi_output,
  mpi_specs                  = NULL,
  filename                   = NULL,
  include_deprivation_matrix = TRUE,
  include_specs              = FALSE,
  ...
) {
  check_old_dotted_args(
    "save_mpi",
    c(".mpi_output", ".mpi_specs", ".filename",
      ".include_deprivation_matrix", ".include_specs"),
    ...
  )

  validate_mpi_specs(mpi_specs)

  # Resolve output path
  if (is.null(filename)) {
    file <- file.path(getwd(), "MPI Results.xlsx")
  } else {
    file <- filename
    if (!grepl("\\.xlsx$", file)) {
      file <- paste0(file, ".xlsx")
    }
  }

  # Build flat named sheet list
  sheets <- list()
  sheets[["MPI"]]                          <- mpi_output$index
  sheets[["Contribution by dimension"]]    <- mpi_output$contribution
  sheets[["Headcount ratio"]]              <- mpi_output$headcount_ratio

  if (include_deprivation_matrix && "deprivation_matrix" %in% names(mpi_output)) {
    dm      <- mpi_output$deprivation_matrix
    dm_names <- names(dm)
    sheets[["Uncensored deprivation matrix"]] <- dm$uncensored

    dm_cutoffs <- dm_names[dm_names != "uncensored"]
    for (k in dm_cutoffs) {
      label <- paste0("Dep. matrix (k = ", stringr::str_remove(k, "^k_"), "%)")
      sheets[[label]] <- dm[[k]]
    }
  }

  if (include_specs) {
    sheets[["MPI Specification"]] <- mpi_specs |>
      dplyr::select(
        dplyr::any_of(c("dimension", "indicator", "variable", "weight", "description"))
      ) |>
      dplyr::rename_with(to_title_case)
  }

  sheets <- flatten_mpi_sheet_list(sheets)
  tsg::write_xlsx(sheets, path = file)

  return(normalizePath(file, mustWork = FALSE))
}


#' Flatten nested MPI sheet list for Excel export
#'
#' @param sheets Named list of data frames or named lists of data frames.
#' @return A flat named list of data frames.
#' @keywords internal
#'
flatten_mpi_sheet_list <- function(sheets) {
  out <- list()
  for (nm in names(sheets)) {
    val <- sheets[[nm]]
    if (is.data.frame(val)) {
      out[[substr(nm, 1, 31)]] <- val
    } else if (is.list(val)) {
      for (sub_nm in names(val)) {
        label <- if (grepl("^k_", sub_nm)) {
          paste0(nm, " (", stringr::str_remove(sub_nm, "^k_"), "%)")
        } else {
          paste0(nm, " (", sub_nm, ")")
        }
        out[[substr(label, 1, 31)]] <- val[[sub_nm]]
      }
    }
  }
  out
}
