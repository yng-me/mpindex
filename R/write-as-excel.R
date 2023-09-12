write_as_excel <- function(
  .data,
  wb = NULL,
  sheet = set_sheet_name(wb),
  append_to_existing_sheet = F,
  title = NULL,
  subtitle = NULL,
  description = NULL,
  footnote = NULL,
  source_note = NULL,
  start_col = 2,
  start_row = 2,
  options = NULL,
  .names_separator = '>',
  cols_with_decimal_format = NULL,
  format_precision = 2,
  overwrite = TRUE,
  ...
) {

  depth <- NULL
  col_from <- NULL
  type <- NULL
  n <- NULL
  r <- NULL
  value <- NULL

  start_row_init <- start_row
  restart_row <- start_row_init

  if(!exists('wb') | is.null(wb)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontName = 'Arial', fontSize = 10)
  }


  col_length <- ncol(.data) + start_col - 1
  col_range <- start_col:col_length

  if(sheet %in% names(wb) & append_to_existing_sheet == F) {
    openxlsx::removeWorksheet(wb = wb, sheet = sheet)
    warning('Overwrite existing sheet with the same sheet name provided.')
  }

  if(!(sheet %in% names(wb)) & append_to_existing_sheet == F) {

    openxlsx::addWorksheet(
      wb = wb,
      sheet = sheet,
      gridLines = F,
      ...
    )
  }

  if(!is.null(title)) {
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = title,
      startCol = start_col,
      startRow = restart_row,
      ...
    )

    openxlsx::mergeCells(
      wb = wb,
      sheet = sheet,
      cols = start_col:col_length,
      rows = restart_row
    )

    openxlsx::addStyle(
      wb = wb,
      sheet = sheet,
      style = openxlsx::createStyle(
        fontSize = 13,
        indent = 0,
        textDecoration = 'bold'
      ),
      rows = restart_row,
      cols = start_col,
      gridExpand =  T,
      stack =  T
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet,
      rows = restart_row,
      heights = 30
    )

    restart_row <- restart_row + 1

  }

  if(!is.null(description)) {
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = description,
      startCol = start_col,
      startRow = restart_row,
      ...
    )

    openxlsx::mergeCells(
      wb = wb,
      sheet = sheet,
      cols = start_col:col_length,
      rows = restart_row
    )

    restart_row <- restart_row + 1
  }

  if(!is.null(subtitle)) {
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = subtitle,
      startCol = start_col,
      startRow = restart_row + 1,
      ...
    )

    openxlsx::addStyle(
      wb = wb,
      sheet = sheet,
      style = openxlsx::createStyle(
        fontSize = 12,
        indent = 0,
        valign = 'center',
        textDecoration = 'bold'
      ),
      rows = restart_row + 1,
      cols = start_col,
      gridExpand =  T,
      stack =  T
    )

    openxlsx::setRowHeights(
      wb = wb,
      sheet = sheet,
      rows = restart_row + 1,
      heights = 35
    )

    restart_row <- restart_row + 1
  }

  merge_colnames <- extract_column_names(
    .data,
    start_col = start_col,
    start_row = restart_row,
    .names_separator = .names_separator
  )

  row_depth <- max(merge_colnames$depth)

  if(row_depth == 1) {

    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = .data,
      startCol = start_col,
      startRow = row_depth + restart_row,
      borders = 'all',
      borderStyle = 'dashed',
      borderColour = 'gray',
      ...
    )

  } else {

    row_depth_all <- row_depth + 1
    row_depth_inner <- row_depth - 1

    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = .data,
      startCol = start_col,
      startRow = row_depth_all + restart_row,
      colNames = F,
      borders = 'all',
      borderStyle = 'dashed',
      borderColour = 'gray',
      ...
    )

    merge_rows <- merge_colnames |> dplyr::filter(depth == 1)

    for(m in 1:nrow(merge_rows)) {

      row_from <- merge_rows$depth[m]

      row_range <- row_from:row_depth + restart_row
      openxlsx::writeData(
        wb = wb,
        sheet = sheet,
        x = merge_rows$value[m],
        startRow = row_from + restart_row,
        startCol = merge_rows$col_from[m],
        ...
      )

      openxlsx::mergeCells(
        wb = wb,
        sheet = sheet,
        cols = merge_rows$col_from[m],
        rows = row_range
      )
    }

    # TOP COLUMN HEADER
    top_col <- merge_colnames |>
      dplyr::filter(row_from == restart_row, depth > 1) |>
      dplyr::mutate(col_to = col_from + r - 1) |>
      dplyr::distinct(value, .keep_all = T)

    for(i in 1:nrow(top_col)) {

      top_col_from <- top_col$col_from[i]
      top_col_to <- top_col$col_to[i]

      openxlsx::writeData(
        wb = wb,
        sheet = sheet,
        x = top_col$value[i],
        startRow = restart_row + 1,
        startCol = top_col$col_from[i],
        ...
      )

      openxlsx::mergeCells(
        wb = wb,
        sheet = sheet,
        cols = top_col_from:top_col_to,
        rows = restart_row + 1
      )
    }

    # MIDDLE COLUMN HEADER
    if(row_depth_inner > 1) {

      for(j in 2:row_depth_inner) {

        inner <- merge_colnames |> dplyr::filter(row_from == j + restart_row - 1)

        inner_seq <- tibble::as_tibble_col(
          increment_inner_depth(inner$value),
          column_name = 'seq'
        )

        inner_col <- inner |>
          dplyr::mutate(col_to = col_from + max(merge_colnames$r) - (r + 1)) |>
          tibble::add_column(inner_seq) |>
          dplyr::group_by(value, seq) |>
          dplyr::summarise(
            min = min(col_from),
            max = max(col_from),
            .groups = 'drop'
          ) |>
          dplyr::arrange(seq)

        openxlsx::writeData(
          wb = wb,
          sheet = sheet,
          x = t(inner$value),
          startCol = min(inner$col_from),
          startRow = j + restart_row,
          colNames = F,
          ...
        )

        for(k in 1:nrow(inner_col)) {

          inner_col_from <- inner_col$min[k]
          inner_col_to <- inner_col$max[k]

          openxlsx::mergeCells(
            wb = wb,
            sheet = sheet,
            cols = inner_col_from:inner_col_to,
            rows = j + restart_row
          )
        }

      }
    }

    # BOTTOM COLUMN HEADER
    bottom_col <- merge_colnames |>
      dplyr::filter(row_from == row_depth + restart_row - 1)

    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = t(bottom_col$value),
      startCol = min(bottom_col$col_from),
      startRow = row_depth + restart_row,
      colNames = F,
      ...
    )

    # FINAL MERGE
    last_merges <- merge_colnames |> dplyr::filter(stringr::str_trim(value) == '')
    if(nrow(last_merges) > 0) {
      for(li in 1:nrow(last_merges)) {

        row_merge_final_from <- restart_row + last_merges$row_from[li] - 3
        row_merge_final_to <- restart_row + last_merges$row_from[li] - 2

        openxlsx::removeCellMerge(
          wb = wb,
          sheet = sheet,
          cols = last_merges$col_from[li],
          rows = row_merge_final_from
        )

        openxlsx::mergeCells(
          wb = wb,
          sheet = sheet,
          cols = last_merges$col_from[li],
          rows = row_merge_final_from:row_merge_final_to
        )

      }
    }

  }

  row_length <- nrow(.data) + restart_row + row_depth
  start_row_note <- row_length + 2

  if(!is.null(footnote)) {
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = footnote,
      startCol = start_col,
      startRow = start_row_note
    )

  }

  if(!is.null(source_note)) {
    if(!is.null(footnote)) {
      start_row_note <- start_row_note + 2
    }
    openxlsx::writeData(
      wb = wb,
      sheet = sheet,
      x = source_note,
      startCol = start_col,
      startRow = start_row_note
    )
  }

  set_export_facade(
    wb = wb,
    sheet = sheet,
    header_depth = row_depth,
    start_row_init = start_row_init,
    start_row = restart_row + 1,
    start_col = start_col,
    end_row = row_length,
    end_col = col_length,
    start_row_note = start_row_note,
    decimal_format_cols = cols_with_decimal_format,
    format_precision = format_precision,
    options = options
  )

  return(list(start_row = start_row_note - 1))

}
