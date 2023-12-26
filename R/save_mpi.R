#' Save MPI output
#'
#' @description Save the MPI output into an Excel file format.
#'
#' @param .mpi_output An object derived from \link[mpindex]{compute_mpi}.
#' @param .mpi_specs MPI specifications defined in \code{\link[mpindex]{define_mpi_specs}}.
#' @param .filename Output filename
#' @param .formatted_output Whether formatting is to be applied to the output.
#' @param .include_table_summary NOT YET IMPLEMENTED. Whether to include summary information in the generated output.
#' @param .include_specs Whether to include MPI specification in the generated output.
#'
#' @return Returns the file location of the output generated.
#' @export
#'
#' @examples
#' \dontrun{
#' # It requires an MPI output (list type) in the first argument
#' save_mpi(mpi_result, .filename = "MPI Sample Output")
#' }
#'
save_mpi <- function(
  .mpi_output,
  .mpi_specs = getOption("mpi_specs"),
  .filename = NULL,
  .formatted_output = TRUE,
  .include_table_summary = TRUE,
  .include_specs = FALSE
) {

  validate_mpi_specs(.mpi_specs)
  spec_attr <- attributes(.mpi_specs)

  tb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(tb, fontName = "Arial", fontSize = 10)


  if(.formatted_output) {

    save_mpi_formatted(
      tb,
      set_mpi_sheets(.mpi_output),
      nrow(.mpi_specs)
    )

  } else {

    save_mpi_unformatted(
      tb,
      set_mpi_sheets(.mpi_output),
      nrow(.mpi_specs)
    )
  }

  if(.include_specs) {
    save_mpi_specs(tb, .mpi_specs, .formatted_output)
  }

  if (is.null(.filename)) file <- "MPI Results.xlsx"
  else file <- .filename

  if (!grepl("\\.xlsx$", .filename)) {
    file <- paste0(.filename, ".xlsx")
  }

  openxlsx::saveWorkbook(tb, file, overwrite = TRUE)

  return(paste0(getwd(), "/", file))
}


save_mpi_formatted <- function(.wb, .data, .indicator_count) {

  tb_sheets <- names(.data)

  start_row <- 2
  start_col <- 2

  for (i in seq_along(tb_sheets)) {

    tb_sheet <- tb_sheets[i]
    df <- .data[[tb_sheet]]

    if (inherits(df, "list")) {
      restart_row <- start_row
      df_names <- names(df)

      for (j in seq_along(df_names)) {

        df_j <- df[[j]]

        decimal_format <- set_decimal_format(df_j, tb_sheet, .indicator_count)

        df_name <- df_names[j]
        if (df_name == "uncensored" | df_name == "censored") {
          df_name <- to_title_case(df_name)
        } else if (grepl("^k_", df_name)) {
          df_name <- paste0(
            stringr::str_replace(df_name, "^k_", ""),
            "% poverty cutoff"
          )
        }

        write_as_excel_here <- function(.df, ...) {
          .df |>
            write_as_excel(
              ...,
              wb = .wb,
              sheet = tb_sheet,
              subtitle = df_name,
              format_precision = 3,
              # .names_separator = spec_attr$names_separator,
              cols_with_decimal_format = decimal_format,
              start_row = restart_row
            )
        }

        if (j == 1) {
          tb_for_list <- df_j |> write_as_excel_here(title = tb_sheet)
        } else {
          tb_for_list <- df_j |> write_as_excel_here(append_to_existing_sheet = T)
        }

        restart_row <- tb_for_list$start_row
      }
    } else {

      decimal_format <- set_decimal_format(df, tb_sheet, .indicator_count)

      write_as_excel(
        df,
        wb = .wb,
        sheet = tb_sheet,
        title = tb_sheet,
        cols_with_decimal_format = decimal_format,
        format_precision = 3,
        start_col = start_col
      )
    }
  }
}

save_mpi_unformatted <- function(.wb, .data, .indicator_count) {

  tb_sheets <- names(.data)

  for (i in seq_along(tb_sheets)) {

    tb_sheet <- tb_sheets[i]
    df <- .data[[tb_sheet]]

    if (inherits(df, "list") && !grepl("[Dd]eprivation matrix", tb_sheet)) {

      df_list <- list()
      df_names <- names(df)

      for (j in seq_along(df_names)) {
        df_name <- df_names[j]
        df_list[[j]] <- df[[df_name]] |>
          dplyr::mutate(
            cuttoff = dplyr::if_else(
              to_lowercase(df_name) == 'uncensored' | to_lowercase(df_name) == 'censored',
              to_title_case(df_name),
              paste0(stringr::str_remove(df_name, "^k_"), "%")
            ),
            .before = 1
          )
      }

      df <- do.call("rbind", df_list)
    }

    decimal_format <- set_decimal_format(df, tb_sheet, .indicator_count)

    write_as_excel(
      df,
      wb = .wb,
      sheet = tb_sheet,
      cols_with_decimal_format = decimal_format,
      format_precision = 3,
      start_row = 1,
      start_col = 1,
      format_output = FALSE
    )

  }
}

save_mpi_specs <- function(.wb, .mpi_specs, .formatted_output) {

  start_row <- 1
  start_col <- 1

  specs <- .mpi_specs |>
    dplyr::select(dplyr::any_of(c("dimension", "indicator", "variable", "weight", "description")))

  if(.formatted_output) {

    start_row <- 2
    start_col <- 2

    specs <- specs |> dplyr::rename_with(to_title_case)
  }



  facade <- list()
  facade$col_width_all = rep(18, ncol(specs))
  if("description" %in% to_lowercase(names(specs))) {
    facade$col_width_last = 132
  }

  write_as_excel(
    specs,
    wb = .wb,
    sheet = "MPI Specification",
    title = "MPI Specification",
    cols_with_decimal_format = which(to_lowercase(names(specs)) == "weight"),
    format_precision = 3,
    format_output = .formatted_output,
    start_col = start_col,
    start_row = start_row,
    facade = facade
  )
}

