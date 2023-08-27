#' Compute headcount ratio
#'
#' @param .data data
#' @param ... others
#' @param censored boolean
#'
#' @return
#'
#' @export
#'
#' @examples
#'

compute_headcount_ratio <- function(.data, ..., censored = T) {

  .data <- .data |>
    dplyr::filter(cutoff == recommended_cutoff)

  pattern <- '^d[1-5]_.*_u$'
  if(censored == T) pattern <- '^d[1-5]_.*_u_k$'

  count <- .data |>
    dplyr::group_by(...) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::select(n)

  df <- .data |>
    dplyr::group_by(...) |>
    dplyr::add_count() |>
    dplyr::summarise_at(dplyr::vars(dplyr::matches(pattern)), mean) |>
    dplyr::select(..., n, matches(pattern))

  return(df)
}
