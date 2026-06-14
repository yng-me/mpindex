#' Save MPI output
#'
#' @description Save the MPI output to an Excel file using the
#' \href{https://cran.r-project.org/package=tsg}{tsg} package for
#' publication-ready table formatting.
#'
#' @param mpi_output An object derived from \link[mpindex]{compute_mpi}.
#' @param mpi_specs MPI specifications defined in
#'   \code{\link[mpindex]{define_mpi_specs}}.
#' @param ... Reserved; passing old dotted names triggers a helpful error.
#' @param filename Output filename. The \code{.xlsx} extension is added
#'   automatically when missing. Defaults to \code{"MPI Results.xlsx"} in the
#'   current working directory.
#' @param include_specs Whether to include MPI specification as a separate
#'   sheet. Defaults to \code{FALSE}.
#' @param overall_label  Overall label to assign when grouping is defined in \code{compute_mpi()} through \code{by} argument. Default is \code{"Overall"}. Accepts vector of elements matching the number of grouping variables defined.
#' @param facade \code{tsg} facade (see \link[tsg]{add_facade}).
#'
#' @return Returns the normalized file path of the generated Excel file.
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
  mpi_specs,
  ...,
  filename                   = NULL,
  include_specs              = FALSE,
  overall_label              = "Overall",
  facade = tsg::get_tsg_facade()
) {

  check_old_dotted_args(
    "save_mpi",
    c(".mpi_output", ".mpi_specs", ".filename", ".include_specs"),
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

  sheets[["MPI"]] <- bind_sheets(mpi_output, "index", overall_label)
  sheets[["Headcount ratio"]] <- bind_sheets(mpi_output, "headcount_ratio", overall_label)
  sheets[["Contribution by dimension"]] <- bind_sheets(mpi_output, "contribution", overall_label)

  # -------------------------------------
  # Include deprivation matrix
  if ("deprivation_matrix" %in% names(mpi_output)) {
    dm <- mpi_output$deprivation_matrix
    dm_names <- names(dm)
    sheets[["Deprivation matrix (uncensored)"]] <- dm$uncensored

    dm_cutoffs <- dm_names[dm_names != "uncensored"]
    for (k in dm_cutoffs) {
      label <- paste0("Deprivation matrix (k = ", stringr::str_pad(stringr::str_remove(k, "^k_"), width = 2, pad = "0"), "%)")
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

  tsg::write_xlsx(sheets, path = file, facade = facade)
  return(normalizePath(file, mustWork = FALSE))
}



get_column_labels <- function(data) {

  labels <- NULL
  for(i in names(data)) {

    label <- attributes(data[[i]])$label
    if(is.null(label)) {
      label <- i
    }

    labels <- c(labels, label)

  }

  names(labels) <- names(data)

  labels

}

set_column_labels <- function(data, labels) {

  for(i in names(data)) {
    attr(data[[i]], "label") <- labels[[i]]
  }

  data

}

tidy_poverty_cutoff <- function(data) {

  if(length(data) == 1) return(data[[1]])

  data |>
    dplyr::bind_rows(.id = "poverty_cutoff") |>
    dplyr::mutate(
      poverty_cutoff = dplyr::if_else(
        grepl("^k_", poverty_cutoff),
        paste0(stringr::str_pad(stringr::str_remove(poverty_cutoff, "^k_"), width = 2, pad = "0"), "%"),
        to_title_case(poverty_cutoff)
      )
    )

}

bind_sheets <- function(data, key, overall_label) {

  df <- data[[key]]
  class(df) <- "list"

  # Single poverty cutoff and with no grouping
  if(length(df) == 1 && is.null(data$overall)) { return(df[[1]]) }

  data_labels <- get_column_labels(df[[1]])
  label_offset <- 0L

  if(length(df) > 1) {
    label_offset <- 1L
    data_labels <- c("poverty_cutoff" = "Poverty cutoff", data_labels)
  }

  # Multiple poverty cutoffs and with no grouping
  if(length(df) > 1 && is.null(data$overall)) {

    res <- df |>
      tidy_poverty_cutoff() |>
      set_column_labels(labels = data_labels)

    return(res)

  }

  grouping <- attributes(data)$grouping

  res <- list()

  for(k in names(df)) {

    if(grepl("^k_", k)) {
      k_label <- paste0("Poverty cutoff, k = ", stringr::str_pad(stringr::str_remove(k, "^k_"), width = 2, pad = "0"), "%")
    } else {
      k_label <- to_title_case(k)
    }

    res_k <- dplyr::bind_rows(df[[k]], data$overall[[key]][[k]])

    if(length(grouping) > 1 & length(overall_label) == 1) {
      overall_label <- rep(overall_label, length(grouping))
    }

    for(i in seq_along(grouping)) {
      res_k[nrow(res_k), grouping[i]] <- overall_label[i]
    }

    res[[k_label]] <- set_column_labels(res_k, labels = data_labels)

  }

  attr(res, "groups") <- "poverty_cutoff"
  if(length(data[[key]]) == 1) { res <- res[[1]] }
  tsg::add_facade(res, table.lastRowBold = TRUE)

}

