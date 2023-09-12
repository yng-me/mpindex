bind_list <- function(.data, .list, .by) {

  list_names <- names(.list)

  for(i in seq_along(list_names)) {
    .data <- .data |>
      dplyr::left_join(.list[[list_names[i]]], by = .by)
  }
  return(.data)
}

rename_indicators <- function(.data, .mpi_specs = getOption('mpi_specs')) {

  value <- NULL
  label <- NULL
  deprivation_score <- NULL

  .mpi_colnames <- names(.data) |>
    dplyr::as_tibble() |>
    dplyr::mutate(variable_name = value) |>
    dplyr::left_join(.mpi_specs$indicators, by = 'variable_name') |>
    dplyr::mutate(label = dplyr::if_else(is.na(label), value, label)) |>
    dplyr::pull(label)

  colnames(.data) <- .mpi_colnames

  if('n' %in% names(.data)) {
    .data <- .data |>
      rename_n(.mpi_specs$unit_of_analysis)
  }

  if('deprivation_score' %in% names(.data)) {
    .data <- .data |>
      dplyr::rename('Deprivation score' = deprivation_score)
  }

  return(.data)
}

rename_n <- function(.data, .label) {
  n <- NULL
  `:=` <- NULL
  .data |>
    dplyr::rename(!!as.name(paste0('Number of ', .label)) := n)
}

to_enquo_str <- function(to_str) {
  stringr::str_remove(rlang::expr_text(rlang::enquo(to_str)), '~')
}

set_cutoff_label <- function(.value) {
  paste0(round(.value * 100), '% poverty cutoff')
}

set_k_label <- function(.value) {
  paste0('k_', round(.value * 100))
}

set_dep_label <- function(.value, .index) {
  if(length(.value) == 1) {
    .v <- 'censored'
  } else if (length(.value) > 1) {
    .v <- .value[.index]
  } else {
    .v <- 'na'
  }
  return(.v)
}

# ------------------------------------------------------------------------------
increment_inner_depth <- function(vec) {
  c <- vec[1]
  s <- 1
  for(i in 2:length(vec)) {
    s_n <- s[i - 1]
    if(c == vec[i]) {
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
    decimal_format_cols = NULL,
    format_precision = 2,
    options = NULL
) {

  options_default <- list()

  options_default$col_width_first <- 38
  options_default$col_width_all <- 20
  options_default$row_height <- 20
  options_default$row_height_header <- 32


  options_default$style_indent <- openxlsx::createStyle(
    indent = 1,
    numFmt = '#,##0',
    valign = 'center'
  )

  options_default$style_header <- openxlsx::createStyle(
    wrapText = T,
    valign = 'center',
    fgFill = '#f5f5f5',
    border = c('top', 'bottom', 'left', 'right'),
    borderStyle = 'dashed',
    borderColour = 'gray'
  )

  options_default$border_right_outer <- openxlsx::createStyle(
    border = 'right',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_left_outer <- openxlsx::createStyle(
    border = 'left',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_top_outer <- openxlsx::createStyle(
    border = 'top',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_bottom_outer <- openxlsx::createStyle(
    border = 'bottom',
    borderColour = '#757575',
    borderStyle = 'medium'
  )

  options_default$border_header <- openxlsx::createStyle(
    border = 'bottom',
    borderColour = '#757575',
    borderStyle = 'double'
  )

  options <- c(options, options_default)

  # Default width of the first column
  start_col_width <- start_col - 1
  start_col_plus <- start_col + 1

  # Start col
  openxlsx::setColWidths(..., cols = 1:start_col_width, widths = rep(2, start_col_width))
  openxlsx::setColWidths(..., cols = start_col, widths = options$col_width_first)

  # Middle cols
  if(length(options$col_width_all) > length(start_col_plus:end_col)) {
    col_width_all_fit <- options$col_width_all[1:length(start_col_plus:end_col)]
  } else {
    col_width_all_fit <- options$col_width_all
  }
  openxlsx::setColWidths(..., cols = start_col_plus:end_col, widths = col_width_all_fit)

  # End col
  if(!is.null(options$col_width_last) | length(is.na(options$col_width_last)) > 0) {
    openxlsx::setColWidths(..., cols = end_col, widths = options$col_width_last)
  }

  end_row_header <- header_depth + start_row - 1
  start_row_tb <- end_row_header + 1


  openxlsx::setRowHeights(..., rows = 1, heights = 15)
  openxlsx::setRowHeights(..., rows = start_row:end_row_header, heights = options$row_height_header)
  openxlsx::setRowHeights(..., rows = start_row_tb:end_row, heights = options$row_height)

  # if(!is.null())
  openxlsx::setRowHeights(..., rows = start_row_tb:end_row, heights = options$row_height)

  openxlsx::addStyle(
    ...,
    style = options$style_header,
    rows = start_row:end_row_header,
    cols = start_col:end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$style_indent,
    rows = start_row:end_row,
    cols = start_col:end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_right_outer,
    rows = start_row:end_row,
    cols = end_col,
    gridExpand = T,
    stack = T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_left_outer,
    rows = start_row:end_row,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_top_outer,
    rows = start_row,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_bottom_outer,
    rows = end_row,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = options$border_header,
    rows = end_row_header,
    cols = start_col:end_col,
    gridExpand =  T,
    stack =  T
  )

  openxlsx::addStyle(
    ...,
    style = openxlsx::createStyle(
      textDecoration = NULL,
      fontColour = '#07403b',
      fontSize = 9
    ),
    rows = 1,
    cols = start_col,
    gridExpand =  T,
    stack =  T
  )

  # Decimal
  if(length(decimal_format_cols) > 0) {

    add_decimal_style <- function(cols, precise) {
      openxlsx::addStyle(
        ...,
        style = openxlsx::createStyle(
          indent = 1,
          numFmt = paste0('#,#0.', paste0(rep(0, as.integer(precise)), collapse = ''))
        ),
        rows = end_row_header:end_row,
        cols = cols + start_col - 1,
        gridExpand = T,
        stack = T
      )
    }

    if(inherits(decimal_format_cols, 'list')) {

      for(d in 1:length(format_precision)) {
        add_decimal_style(decimal_format_cols[[d]], format_precision[d])
      }

    } else {

      add_decimal_style(decimal_format_cols, format_precision)

    }

  }

}


extract_column_names <- function(
  df,
  start_col = 1,
  start_row = 1,
  .names_separator = '>'
) {

  value <- NULL
  n <- NULL
  col_from <- NULL
  row_from <- NULL
  data <- NULL

  dplyr::as_tibble(names(df)) |>
    dplyr::mutate(
      value = stringr::str_split(value, .names_separator),
      col_from = 1:n()
    ) |>
    dplyr::mutate(col_from = col_from + start_col - 1) |>
    tidyr::unnest(value) |>
    dplyr::group_by(col_from) |>
    dplyr::mutate(row_from = 1:n()) |>
    dplyr::mutate(row_from = row_from + start_row - 1) |>
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
  sheet <- paste0('Sheet ', length(names(wb)) + 1)
  return(sheet)
}

set_mpi_sheets <- function(.mpi_output) {

  mpi_list <- list()
  mpi_list[['MPI']] <- .mpi_output$index
  mpi_list[['Contribution by dimension']] <- .mpi_output$contribution
  mpi_list[['Headcount ratio']] <- .mpi_output$headcount_ratio

  if('deprivation_matrix' %in% names(.mpi_output)) {
    dm <- .mpi_output$deprivation_matrix
    dm_names <- names(.mpi_output$deprivation_matrix)
    mpi_list[['Uncensored deprivation matrix']] <- dm$uncensored

    if(length(dm_names) > 2) {
      dm_censored <- dm_names[-1]
      for(i in seq_along(dm_censored)) {
        dm_name <- paste0(stringr::str_replace(dm_censored[i], 'k_', 'Deprivation matrix (k = '), '%)')
        mpi_list[[dm_name]] <- dm[[dm_censored[i]]]
      }
    } else {

      mpi_list[['Censored deprivation matrix']] <- dm$censored

    }
  }
  return(mpi_list)
}


set_decimal_format <- function(.data, .sheet, .n) {
  if(.sheet == 'MPI') {
    mpi_s <- ncol(.data) - 2
    decimal_format <- mpi_s:ncol(.data)
  } else if (.sheet == 'Contribution by dimension' | .sheet == 'Headcount ratio') {
    mpi_d <- ncol(.data) - .n + 1
    decimal_format <- mpi_d:ncol(.data)
  } else if (grepl('deprivation matrix', .sheet, ignore.case = T)) {
    decimal_format <- which(names(.data) == 'Deprivation score')
  } else {
    decimal_format <- NULL
  }
  return(decimal_format)
}
