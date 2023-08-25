compute_headcount_ratio <- function(.data, ..., censored = T) {

  .data <- .data %>%
    filter(cutoff == recommended_cutoff)

  pattern <- '^d[1-5]_.*_u$'
  if(censored == T) pattern <- '^d[1-5]_.*_u_k$'

  count <- .data %>%
    group_by(...) %>%
    count() %>%
    ungroup() %>%
    select(n)

  df <- .data %>%
    group_by(...) %>%
    summarise_at(vars(matches(pattern)), mean) %>%
    add_column(count) %>%
    select(..., n, matches(pattern))

  return(df)
}
