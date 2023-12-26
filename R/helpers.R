bind_list <- function(.data, .list, .by) {
  list_names <- names(.list)

  for (i in seq_along(list_names)) {
    .data <- .data |>
      dplyr::left_join(.list[[list_names[i]]], by = .by)
  }
  return(.data)
}

rename_indicators <- function(.data, .mpi_specs = getOption("mpi_specs")) {

  value <- NULL
  label <- NULL
  dimension <- NULL
  indicator <- NULL
  deprivation_score <- NULL

  mpi_colnames <- names(.data) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable_name = value) |>
    dplyr::left_join(.mpi_specs, by = "variable_name") |>
    dplyr::mutate(label = dplyr::if_else(
      is.na(label),
      to_title_case(value),
      indicator
      # paste0(indicator, " (", dimension, ")")
    ))

  for(i in seq_along(mpi_colnames$value)) {
    mpi_col <- mpi_colnames$value[i]
    attr(.data[[mpi_col]], "label") <- mpi_colnames$label[i]

    if(!is.na(mpi_colnames$dimension[i])) {
      attr(.data[[mpi_col]], "dimension") <- mpi_colnames$dimension[i]
    }

    if(!is.na(mpi_colnames$indicator[i])) {
      attr(.data[[mpi_col]], "indicator") <- mpi_colnames$indicator[i]
    }
  }

  if ("n" %in% names(.data)) {
    attr_specs <- attributes(.mpi_specs)
    .data <- .data |>
      rename_n(attr_specs$unit_of_analysis)
  }

  if ("uuid" %in% names(.data)) {
    attr(.data$uuid, "label") <- "UUID"
  }

  if ("deprivation_score" %in% names(.data)) {
    attr(.data$deprivation_score, "label") <- "Deprivation score"
  }

  .data <- .data |>
    dplyr::rename_with(~ stringr::str_remove_all(., '^d\\d{2}_i\\d{2}_'))

  return(.data)
}

rename_n <- function(.data, .label) {

  `:=` <- NULL
  n <- NULL

  if(is.null(.label)) .label <- "cases"

  mpi_colname <- paste0("number_of_", to_lowercase(.label))

  .data <- .data |>
    dplyr::rename(!!as.name(mpi_colname) := n)

  attr(.data[[mpi_colname]], "label") <- paste0("Number of ", .label)

  return(.data)
}

to_enquo_str <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), "~")
}

set_cutoff_label <- function(.value) {
  paste0(round(.value * 100), "% poverty cutoff")
}

set_k_label <- function(.value) {
  paste0("k_", round(.value * 100))
}

set_dep_label <- function(.value, .index) {
  if (length(.value) == 1) {
    .v <- "censored"
  } else if (length(.value) > 1) {
    .v <- .value[.index]
  } else {
    .v <- "na"
  }
  return(.v)
}

# ------------------------------------------------------------------------------
increment_inner_depth <- function(vec) {
  c <- vec[1]
  s <- 1
  for (i in 2:length(vec)) {
    s_n <- s[i - 1]
    if (c == vec[i]) {
      s <- c(s, s_n)
    } else {
      s <- c(s, s_n + 1)
    }
    c <- vec[i]
  }
  return(s)
}


set_export_facade <- function(
  ...,
  header_depth,
  start_row_init,
  start_row,
  start_col,
  end_row,
  end_col,
  start_row_note,
  format_output = FALSE,
  decimal_format_cols = NULL,
  format_precision = 2,
  facade = NULL
) {

  facade_default <- list()

  if(format_output) {
    if(header_depth > 1) {
      facade_default$col_width_all <- 10
    } else {
      facade_default$col_width_all <- 18
    }
  } else {
    facade_default$col_width_all <- 15
  }

  facade_default$row_height <- 20
  facade_default$col_width_first <- 36

  if(header_depth > 1) {

    facade_default$row_height_header <- c(20, 36)

  } else {

    facade_default$row_height_header <- 32
  }

  facade_default$style_indent <- openxlsx::createStyle(
    indent = 1,
    numFmt = "#,##0",
    valign = "center"
  )

  facade_default$style_header <- openxlsx::createStyle(
    wrapText = TRUE,
    valign = "center",
    fgFill = "#f5f5f5",
    border = c("top", "bottom", "left", "right"),
    borderStyle = "dashed",
    borderColour = "gray"
  )

  facade_default$center_align <- openxlsx::createStyle(
    halign = "center"
  )

  facade_default$border_right_outer <- openxlsx::createStyle(
    border = "right",
    borderColour = "#757575",
    borderStyle = "medium"
  )

  facade_default$border_left_outer <- openxlsx::createStyle(
    border = "left",
    borderColour = "#757575",
    borderStyle = "medium"
  )

  facade_default$border_top_outer <- openxlsx::createStyle(
    border = "top",
    borderColour = "#757575",
    borderStyle = "medium"
  )

  facade_default$border_bottom_outer <- openxlsx::createStyle(
    border = "bottom",
    borderColour = "#757575",
    borderStyle = "medium"
  )

  facade_default$border_header <- openxlsx::createStyle(
    border = "bottom",
    borderColour = "#757575",
    borderStyle = "double"
  )

  facade <- c(facade, facade_default)

  # Default width of the first column
  start_col_width <- start_col - 1
  start_col_plus <- start_col + 1

  # Start col
  openxlsx::setColWidths(
    ...,
    cols = 1:start_col_width,
    widths = rep(2, start_col_width)
  )
  openxlsx::setColWidths(
    ...,
    cols = start_col,
    widths = facade$col_width_first
  )

  # Middle cols
  if (length(facade$col_width_all) > length(start_col_plus:end_col)) {
    len_to_fit_width <- length(start_col_plus:end_col)
    col_width_all_fit <- facade$col_width_all[1:len_to_fit_width]
  } else {
    col_width_all_fit <- facade$col_width_all
  }
  openxlsx::setColWidths(
    ...,
    cols = start_col_plus:end_col,
    widths = col_width_all_fit
  )

  # End col
  col_with_last <- isTRUE(!is.null(facade$col_width_last))
  col_width_last <- isTRUE(length(is.na(facade$col_width_last)) > 0)
  if (col_with_last | col_width_last) {
    openxlsx::setColWidths(..., cols = end_col, widths = facade$col_width_last)
  }

  end_row_header <- header_depth + start_row - 1
  start_row_tb <- end_row_header + 1


  openxlsx::setRowHeights(..., rows = 1, heights = 15)

  openxlsx::setRowHeights(
    ...,
    rows = start_row:end_row_header,
    heights = facade$row_height_header
  )

  openxlsx::setRowHeights(
    ...,
    rows = start_row_tb:end_row,
    heights = facade$row_height
  )

  openxlsx::setRowHeights(
    ...,
    rows = start_row_tb:end_row,
    heights = facade$row_height
  )

  openxlsx::addStyle(
    ...,
    style = facade$style_header,
    rows = start_row:end_row_header,
    cols = start_col:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$center_align,
    rows = start_row:end_row_header,
    cols = (start_col + 1):end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$style_indent,
    rows = start_row:end_row,
    cols = start_col:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$border_right_outer,
    rows = start_row:end_row,
    cols = end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$border_left_outer,
    rows = start_row:end_row,
    cols = start_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$border_top_outer,
    rows = start_row,
    cols = start_col:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$border_bottom_outer,
    rows = end_row,
    cols = start_col:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    ...,
    style = facade$border_header,
    rows = end_row_header,
    cols = start_col:end_col,
    gridExpand = TRUE,
    stack = TRUE
  )

  if(format_output) {
    openxlsx::addStyle(
      ...,
      style = openxlsx::createStyle(
        textDecoration = NULL,
        fontColour = "#07403b",
        fontSize = 9
      ),
      rows = 1,
      cols = start_col,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Decimal
  if (length(decimal_format_cols) > 0) {
    add_decimal_style <- function(cols, precise) {
      openxlsx::addStyle(
        ...,
        style = openxlsx::createStyle(
          indent = 1,
          numFmt = paste0(
            "#,#0.",
            paste0(rep(0, as.integer(precise)),
              collapse = ""
            )
          )
        ),
        rows = end_row_header:end_row,
        cols = cols + start_col - 1,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    if (inherits(decimal_format_cols, "list")) {
      for (d in seq_along(format_precision)) {
        add_decimal_style(decimal_format_cols[[d]], format_precision[d])
      }
    } else {
      add_decimal_style(decimal_format_cols, format_precision)
    }
  }
}


extract_column_names <- function(
  .data,
  .start_col = 1,
  .start_row = 1,
  .names_separator = ">"
) {
  data <- NULL
  value <- NULL
  col_from <- NULL
  row_from <- NULL
  n <- NULL
  df <- NULL

  headers <- NULL
  df_names <- names(.data)

  for(i in seq_along(df_names)) {
    df_name <- df_names[i]
    attr_name <- attributes(.data[[df_name]])
    label <- attr_name$label
    if(is.null(label)) label <- to_title_case(df_name)
    dimension <- attr_name$dimension
    indicator <- attr_name$indicator

    if(!is.null(dimension) & !is.null(indicator)) {
      # if(!is.na(dimension) & !is.na(indicator)) {
      label <- paste0(dimension, .names_separator, indicator)
      # }
    }
    headers <- c(headers, label)
  }

  dplyr::as_tibble(headers) |>
    dplyr::mutate(
      value = stringr::str_split(value, .names_separator),
      col_from = 1:dplyr::n()
    ) |>
    dplyr::mutate(col_from = col_from + .start_col - 1) |>
    tidyr::unnest(value) |>
    dplyr::group_by(col_from) |>
    dplyr::mutate(row_from = 1:dplyr::n()) |>
    dplyr::mutate(row_from = row_from + .start_row - 1) |>
    tidyr::nest() |>
    dplyr::mutate(depth = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::group_by(value) |>
    tidyr::nest() |>
    dplyr::mutate(r = purrr::map_int(data, nrow)) |>
    tidyr::unnest(data) |>
    dplyr::arrange(col_from)
}

set_sheet_name <- function(wb) {
  sheet <- paste0("Sheet ", length(names(wb)) + 1)
  return(sheet)
}

set_mpi_sheets <- function(.mpi_output) {
  mpi_list <- list()
  mpi_list[["MPI"]] <- .mpi_output$index
  mpi_list[["Contribution by dimension"]] <- .mpi_output$contribution
  mpi_list[["Headcount ratio"]] <- .mpi_output$headcount_ratio

  if ("deprivation_matrix" %in% names(.mpi_output)) {
    dm <- .mpi_output$deprivation_matrix
    dm_names <- names(.mpi_output$deprivation_matrix)
    mpi_list[["Uncensored deprivation matrix"]] <- dm$uncensored

    if (length(dm_names) > 2) {
      dm_censored <- dm_names[-1]
      for (i in seq_along(dm_censored)) {
        dm_name <- paste0(
          stringr::str_replace(
            dm_censored[i],
            "k_", "Deprivation matrix (k = "
          ), "%)"
        )
        mpi_list[[dm_name]] <- dm[[dm_censored[i]]]
      }
    } else {
      mpi_list[["Censored deprivation matrix"]] <- dm$censored
    }
  }
  return(mpi_list)
}


set_decimal_format <- function(.data, .sheet, .n) {
  if (.sheet == "MPI") {
    mpi_s <- ncol(.data) - 2
    decimal_format <- mpi_s:ncol(.data)
  } else if (.sheet %in% c("Contribution by dimension", "Headcount ratio")) {
    mpi_d <- ncol(.data) - .n + 1
    decimal_format <- mpi_d:ncol(.data)
  } else if (grepl("deprivation matrix", .sheet, ignore.case = TRUE)) {
    decimal_format <- which(names(.data) == "Deprivation score" | names(.data) == "deprivation_score")
  } else {
    decimal_format <- NULL
  }
  return(decimal_format)
}
