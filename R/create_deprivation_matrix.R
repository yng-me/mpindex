#' Create deprivation matrix
#'
#' @param .data A data frame or tibble
#' @param ... A tidyselect specification of columns to select
#' @param .mpi_specs A data frame or tibble containing the specifications of the MPI
#' @param .deprivation_cutoffs A list of data frames or tibbles containing the deprivation cutoffs
#'
#' @return A data frame or tibble
#' @export
#'
#' @examples
#'

create_deprivation_matrix <- function(
  .data,
  .deprivation_cutoffs,
  ...,
  .mpi_specs = getOption('mpi_specs')
) {

  cutoff <- NULL
  is_deprived <- NULL
  censored_score <- NULL
  uncensored_score <- NULL

  if(is.null(.mpi_specs)) {
    stop('MPI specifications must be defined first.')
  }

  if(!is.null(.mpi_specs$row_id)) {
    join_by <- .mpi_specs$row_id
  } else {
    join_by <- 'row_id'
    .data <- .data |> tibble::rownames_to_column(var = join_by)
  }

  mpi_dc <- .data |>
    dplyr::select(!!as.name(join_by), ...) |>
    bind_list(.deprivation_cutoffs, join_by)

  mpi_ds <- list()
  p_cutoffs <- .mpi_specs$poverty_cutoffs

  for (k in seq_along(p_cutoffs)) {
    mpi_ds[[k]] <- mpi_dc |>
      dplyr::mutate(
        cutoff = p_cutoffs[k],
        uncensored_score = rowSums(dplyr::across(dplyr::ends_with('_censored')), na.rm = T),
        is_deprived = dplyr::if_else(uncensored_score * 100 >= cutoff, 1, 0),
        censored_score = dplyr::if_else(is_deprived == 1, uncensored_score, 0)
      ) |>
      dplyr::mutate_at(
        dplyr::vars(dplyr::ends_with('_uncensored')),
        list(k = ~ dplyr::if_else(is_deprived == 0, 0, .))
      )
  }

  df <- do.call('rbind', mpi_ds) |>
    dplyr::select(
      dplyr::any_of(join_by),
      ...,
      dplyr::ends_with('_uncensored'),
      dplyr::ends_with('_censored'),
      dplyr::ends_with('_k'),
      uncensored_score,
      censored_score,
      is_deprived,
      cutoff
    )

  return(df)
}
