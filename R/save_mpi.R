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
        if(tb_sheet == 'MPI') {
          mpi_s <- ncol(df_j) - 2
          decimal_format <- mpi_s:ncol(df_j)
        } else if (tb_sheet == 'Contribution by dimension' | tb_sheet == 'Headcount ratio') {
          mpi_d <- ncol(df_j) - nrow(.mpi_specs$indicators) + 1
          decimal_format <- mpi_d:ncol(df_j)
        } else if (grepl('deprivation matrix', tb_sheet, ignore.case = T)) {
          decimal_format <- which(names(df_j) == 'Deprivation score')
        } else {
          decimal_format <- NULL
        }

        df_name <- df_names[j]
        if(df_name == 'uncensored' | df_name == 'censored') {
          df_name <- to_title_case(df_name)
        } else if (grepl('^k_', df_name)) {
          df_name <- paste0(stringr::str_replace(df_name, '^k_', ''), '% poverty cutoff')
        }

        if(j == 1) {

          tb_for_list <- write_as_excel(
            df_j,
            wb = tb,
            title = tb_sheet,
            subtitle = df_name,
            sheet = tb_sheet,
            format_precision = 3,
            cols_with_decimal_format = decimal_format,
            start_row = restart_row,
          )

        } else {

          tb_for_list <- write_as_excel(
            df_j,
            wb = tb,
            subtitle = df_name,
            sheet = tb_sheet,
            start_row = restart_row,
            cols_with_decimal_format = decimal_format,
            format_precision = 3,
            append_to_existing_sheet = T,
          )
        }

        restart_row <- tb_for_list$start_row

      }

    } else {

      if(tb_sheet == 'MPI') {
        mpi_s <- ncol(df) - 2
        decimal_format <- mpi_s:ncol(df)
      } else if (tb_sheet == 'Contribution by dimension' | tb_sheet == 'Headcount ratio') {
        mpi_d <- ncol(df) - nrow(.mpi_specs$indicators) + 1
        decimal_format <- mpi_d:ncol(df)
      } else if (grepl('deprivation matrix', tb_sheet, ignore.case = T)) {
        decimal_format <- which(names(df) == 'Deprivation score')
      } else {
        decimal_format <- NULL
      }

      write_as_excel(
        df,
        wb = tb,
        sheet = tb_sheet,
        title = tb_sheet,
        cols_with_decimal_format = decimal_format,
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
