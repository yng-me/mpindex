to_lowercase <- function(...) tolower(...)

to_uppercase <- function(...) toupper(...)

trim_whitespace <- function(...) trimws(...)

to_camel_case <- function(.value) {
  if (!is.character(.value)) {
    stop("Only accepts character")
  }
  .value <- to_lowercase(.value)
  .value <- gsub("[-_\\s]+(.)", "\\U\\1", .value, perl = TRUE)
  .value <- gsub("[-_\\s]", "", .value)
  .value <- gsub("^(.)", "\\U\\1", .value, perl = TRUE)
  .value <- clean_case(.value)
  return(.value)
}

to_kebab_case <- function(.value) {
  if (!is.character(.value)) {
    stop("Only accepts character")
  }
  .value <- to_lowercase(.value)
  .value <- gsub("[-_\\s]+", "-", .value)
  .value <- clean_case(.value)
  .value <- gsub("[\\-]+", "-", .value)
  return(.value)
}

to_snake_case <- function(.value) {
  if (!is.character(.value)) {
    stop("Only accepts character")
  }
  .value <- to_lowercase(.value)
  .value <- gsub("[-_\\s]+", "_", .value)
  .value <- clean_case(.value)
  .value <- gsub("[_]+", "_", .value)

  return(.value)
}

to_pascal_case <- function(.value) {
  if (!is.character(.value)) {
    stop("Only accepts character")
  }
  .value <- to_lowercase(.value)
  .value <- gsub("[-_\\s]+(.)", "\\U\\1", .value, perl = TRUE)
  .value <- gsub("[-_\\s]", "", .value)
  .value <- gsub("^(.)", "\\U\\1", .value, perl = TRUE)
  .value <- clean_case(.value)
  return(.value)
}


to_title_case <- function(.x, .words_to_preserve = NULL) {
  if (!is.vector(.x)) {
    stop("Only accepts vector")
  }

  title <- NULL

  for (i in seq_along(.x)) {
    retained_words <- NULL
    split_words <- strsplit(.x[i], split = " ")

    wtp <- NULL
    if (inherits(.words_to_preserve, "list")) {
      wtp <- .words_to_preserve[[i]]
    } else if (!inherits(.words_to_preserve, "list") && i == 1) {
      wtp <- .words_to_preserve
    }

    if (!is.null(wtp)) {
      sp <- split_words[[1]]
      if ("double" %in% typeof(wtp) || "integer" %in% typeof(wtp)) {
        retained_words <- sp[wtp]
      } else {
        retained_words <- sp[sp %in% wtp]
        wtp <- which(sp %in% retained_words)
      }
    }

    x_split <- gsub("\\s+", " ", trim_whitespace(.x[i]))
    x_split <- gsub(
      "(^|[[:space:]])([[:alpha:]])",
      "\\1\\U\\2",
      tolower(x_split), perl = TRUE
    )
    split_words_final <- strsplit(x_split, split = " ")

    title_new <- sapply(split_words_final, function(w) {
      if (!is.null(wtp)) w[wtp] <- retained_words
      paste(w, collapse = " ")
    })

    title <- c(title, title_new)
  }

  return(title)
}

to_sentence_case <- function(.value, .words_to_preserve = NULL) {
  if (!is.vector(.value)) {
    stop("Only accepts vector")
  }

  gsub(
    "(^|[[:space:]])([[:alpha:]])",
    "\\1\\U\\2",
    tolower(.value), perl = TRUE
  )
}


clean_case <- function(.value) {
  if (!is.character(.value)) {
    stop("Only accepts character")
  }
  .value <- gsub("\\s+", "", .value)
  .value <- gsub("^[-\\s]+", "", .value)
  .value <- gsub("[-\\s]+$", "", .value)
  .value <- gsub("^[_]+", "", .value)
  .value <- gsub("[_]+$", "", .value)
  .value <- gsub("[^a-zA-Z0-9_\\-]+", "", .value)

  return(.value)
}


clean_colnames <- function(.data, .to_lower = TRUE) {
  if (is.vector(.data)) stop("Only accepts data frame")
  cols <- gsub("\\s+\\-?\\s*", "_", names(.data))
  cols <- gsub("[^A-Za-z0-9_]", "", cols)
  cols <- gsub("_+", "_", cols)
  cols <- gsub("_+$", "", cols)
  cols <- gsub("^_+", "", cols)
  if (.to_lower) cols <- to_lowercase(cols)
  colnames(.data) <- cols
  return(.data)
}
