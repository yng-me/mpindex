write_output <- function(
    .mpi_output,
    .mpi_specs = getOption('mpi_specs'),
    .filename = NULL,
    .formatted_output = TRUE,
    .include_table_summary = TRUE,
    .include_specs = FALSE
) {

  wb <- openxlsx::createWorkbook()
  modifyBaseFont(wb, fontName = 'Arial', fontSize = 10)

  .n <- names(.mpi_output)
  for(i in seq_along(.n)) {

    .df <- .mpi_output[[.n[i]]]

    if(inherits(df, 'list')) {
      .m <- names()

      for(j in seq_along(.m)) {

      }

    } else {

    }
  }
}
