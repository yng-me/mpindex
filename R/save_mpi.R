#' Save MPI Output
#'
#' @description Save the MPI ouput into an Excel file format.
#'
#' @param .mpi_output An object derived from \link[mpindex]{compute_mpi}.
#' @param .mpi_specs MPI specifications defined in \code{\link[mpindex]{define_mpi_specs}}.
#' @param .filename Output filename
#' @param .formatted_output Whether formatting is to be applied to the output.
#' @param .include_table_summary NOT YET IMPLEMENTED. Whether to include summary information in the generated output.
#' @param .include_specs NOT YET IMPLEMENTED. Whether to include MPI specification in the generated output.
#'
#' @return Returns the file location of the output generated.
#' @export
#'
#' @examples
#' \dontrun{
#' # It requires an MPI output (list type) in the first argument
#' save_mpi(mpi_result, .filename = 'MPI Sample Output')
#' }

save_mpi <- function(
  .mpi_output,
  .mpi_specs = getOption('mpi_specs'),
  .filename = NULL,
  .formatted_output = TRUE,
  .include_table_summary = TRUE,
  .include_specs = FALSE
) {

  tb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(tb, fontName = 'Arial', fontSize = 10)
  .n <- nrow(.mpi_specs$indicators)
  start_row <- 2
  start_col <- 2

  tb_mpi <- set_mpi_sheets(.mpi_output)
  tb_sheets <- names(tb_mpi)

  for(i in seq_along(tb_sheets)) {

    tb_sheet <- tb_sheets[i]
    df <- tb_mpi[[tb_sheet]]

    if(inherits(df, 'list')) {

      restart_row <- start_row
      df_names <- names(df)

      for(j in 1:length(df_names)) {

        df_j <- df[[j]]

        decimal_format <- set_decimal_format(df_j, tb_sheet, .n)

        df_name <- df_names[j]
        if(df_name == 'uncensored' | df_name == 'censored') {
          df_name <- to_title_case(df_name)
        } else if (grepl('^k_', df_name)) {
          df_name <- paste0(stringr::str_replace(df_name, '^k_', ''), '% poverty cutoff')
        }

        write_as_excel_here <- function(.df_j, ...) {
          .df_j |>
            write_as_excel(
              df_j,
              ...,
              wb = tb,
              sheet = tb_sheet,
              subtitle = df_name,
              format_precision = 3,
              .names_separator = .mpi_specs$names_separator,
              cols_with_decimal_format = decimal_format,
              start_row = restart_row
            )
        }

        if(j == 1) {
          tb_for_list <- write_as_excel_here(title = tb_sheet)
        } else {
          tb_for_list <- write_as_excel_here(append_to_existing_sheet = T)
        }

        restart_row <- tb_for_list$start_row

      }

    } else {

      decimal_format <- set_decimal_format(df, tb_sheet, .n)

      write_as_excel(
        df,
        wb = tb,
        sheet = tb_sheet,
        title = tb_sheet,
        .names_separator = .mpi_specs$names_separator,
        cols_with_decimal_format = ,
        format_precision = 3,
        start_col = start_col
      )
    }
  }

  if(is.null(.filename)) file <- 'Book 1.xlsx'
  else file <- .filename

  if(!grepl('\\.xlsx$', .filename)) {
    file <- paste0(.filename, '.xlsx')
  }

  openxlsx::saveWorkbook(tb, file, overwrite = T)

  return(paste0(getwd(), '/', file))

}
