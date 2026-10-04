bind_list <- function(.data, list_data, by) {
  Reduce(
    function(acc, y) dplyr::left_join(acc, y, by = by, multiple = "first"),
    list_data,
    init = .data
  )
}

rename_indicators <- function(.data, mpi_specs = NULL) {

  mpi_colnames <- dplyr::as_tibble(names(.data)) 
  mpi_colnames <- dplyr::mutate(mpi_colnames, variable_name = value) 
  mpi_colnames <- dplyr::left_join(mpi_colnames, mpi_specs, by = "variable_name") 
  mpi_colnames <- dplyr::mutate(mpi_colnames, label = dplyr::if_else(is.na(label), to_title_case(value), label))

  for (i in seq_along(mpi_colnames$value)) {
    mpi_col <- mpi_colnames$value[i]
    attr(.data[[mpi_col]], "label") <- mpi_colnames$label[i]
  }

  if ("n" %in% names(.data)) {
    .data <- rename_n(.data, attributes(mpi_specs)$unit_of_analysis)
  }

  if ("uuid" %in% names(.data)) {
    .data <- tsg::rename_label(.data, uuid = "UUID")
  }

  if ("deprivation_score" %in% names(.data)) {
    .data <- tsg::rename_label(.data, deprivation_score = "Deprivation score")
  }

  cols <- names(.data)
  cols_start <- cols[!grepl("^d\\d{2}_i\\d{2}_", cols)]
  cols_end <- sort(cols[grepl("^d\\d{2}_i\\d{2}_", cols)])

  .data <- dplyr::select(.data, dplyr::any_of(c(cols_start, cols_end)))

  .data <- dplyr::rename_with(
    .data, 
    ~ sub("^d\\d{2}_i\\d{2}_", "", .)
  )

  

  return(.data)
}

rename_n <- function(.data, label) {

  if (is.null(label)) label <- "cases"

  mpi_colname <- paste0("number_of_", tolower(label))
  .data <- dplyr::rename(.data, !!as.name(mpi_colname) := n)

  attr(.data[[mpi_colname]], "label") <- paste0("Number of ", label)

  return(.data)
}

to_enquo_str <- function(to_str) {
  sub("~", "", rlang::expr_text(rlang::enquo(to_str)))
}

set_cutoff_label <- function(value) {
  paste0(round(value * 100), "% poverty cutoff")
}

set_k_label <- function(value) {
  paste0("k_", round(value * 100))
}

set_dep_label <- function(value, index) {
  value[index]
}

#' @export
print.mpi_output <- function(x, ...) {
  cat("-- MPI Output ---------------------------------------\n")
  idx <- x$index
  for (k in names(idx)) {
    pct <- remove_cutoff_prefix(k)
    cat(sprintf("  Poverty cutoff, k = %s%%\n", pct))
    row <- idx[[k]]
    n_col <- grep("^number_of_", names(row), value = TRUE)[1]
    h_col <- grep("headcount_ratio", names(row), value = TRUE)[1]
    a_col <- grep("^intensity",      names(row), value = TRUE)[1]
    mpi_col <- grep("^mpi",            names(row), value = TRUE)[1]
    if (!is.na(n_col))   cat(sprintf("    n   = %s\n",   format(row[[n_col]][1], big.mark = ",")))
    if (!is.na(h_col))   cat(sprintf("    H   = %.4f\n", row[[h_col]][1]))
    if (!is.na(a_col))   cat(sprintf("    A   = %.4f\n", row[[a_col]][1]))
    if (!is.na(mpi_col)) cat(sprintf("    MPI = %.4f\n", row[[mpi_col]][1]))
  }
  cat("-----------------------------------------------------\n")
  invisible(x)
}

#' @export
print.mpi_list <- function(x, ...) {
  cat("-- MPI by Poverty Cutoff ----------------------------\n")
  for (k in names(x)) {
    cat(sprintf("  Poverty cutoff, k = %s%%\n", remove_cutoff_prefix(k)))
    print(x[[k]])
    cat("---------------------------------------------------\n")
  }
  invisible(x)
}

#' @export
print.mpi_hr_list <- function(x, ...) {
  cat("-- Headcount Ratio by Poverty Cutoff ----------------\n")
  for (k in names(x)) {
    cat(sprintf("  Poverty cutoff, k = %s%%\n", remove_cutoff_prefix(k)))
    print(x[[k]])
    cat("---------------------------------------------------\n")
  }
  invisible(x)
}

#' @export
print.mpi_c_list <- function(x, ...) {
  cat("-- Contribution by Poverty Cutoff -------------------\n")
  for (k in names(x)) {
    cat(sprintf("  Poverty cutoff, k = %s%%\n", remove_cutoff_prefix(k)))
    print(x[[k]])
    cat("---------------------------------------------------\n")
  }
  invisible(x)
}