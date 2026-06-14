check_survey_pkg <- function() {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop(
      "Package 'survey' is required for survey-weighted computation. ",
      "Install it with: install.packages(\"survey\")",
      call. = FALSE
    )
  }
}

# Returns a svydesign or NULL.
resolve_survey_design <- function(
  .data,
  weight        = NULL,
  strata        = NULL,
  cluster       = NULL,
  fpc           = NULL,
  survey_design = NULL
) {
  if (is.null(weight) && is.null(survey_design)) return(NULL)
  check_survey_pkg()

  if (!is.null(survey_design)) {
    if (!inherits(survey_design, "survey.design")) {
      stop("survey_design must be a survey::svydesign() object.", call. = FALSE)
    }
    return(survey_design)
  }

  if (!(weight %in% names(.data))) {
    stop(paste0("Column '", weight, "' not found in .data."), call. = FALSE)
  }

  survey::svydesign(
    ids     = if (!is.null(cluster)) stats::as.formula(paste0("~", cluster)) else ~1,
    strata  = if (!is.null(strata))  stats::as.formula(paste0("~", strata))  else NULL,
    weights = stats::as.formula(paste0("~", weight)),
    fpc     = if (!is.null(fpc))     stats::as.formula(paste0("~", fpc))     else NULL,
    nest    = !is.null(cluster) && !is.null(strata),
    data    = .data
  )
}

# Inject variables from a deprivation matrix into an existing svydesign.
design_with_dm <- function(design, dm, uid) {
  idx      <- match(design$variables[[uid]], dm[[uid]])
  new_vars <- setdiff(names(dm), c(uid, names(design$variables)))
  for (v in new_vars) {
    design$variables[[v]] <- dm[[v]][idx]
  }
  design
}

# Compute weighted H, A, MPI — returns a tibble mirroring the dplyr-path shape.
svy_compute_mpi_summary <- function(design, by_cols, inference, ci_level) {
  has_by <- length(by_cols) > 0
  by_f   <- if (has_by) stats::as.formula(paste("~", paste(by_cols, collapse = " + "))) else NULL

  if (!has_by) {
    h_res <- survey::svymean(~ is_deprived, design, na.rm = TRUE)
    a_res <- survey::svymean(~ deprivation_score, subset(design, is_deprived == 1), na.rm = TRUE)
    h  <- stats::coef(h_res)[["is_deprived"]]
    a  <- stats::coef(a_res)[["deprivation_score"]]
    n  <- nrow(design$variables)
    df <- tibble::tibble(n = n, headcount_ratio = h, intensity = a, mpi = h * a)

    if (inference) {
      h_se   <- as.numeric(survey::SE(h_res))
      a_se   <- as.numeric(survey::SE(a_res))
      mpi_se <- sqrt(a^2 * h_se^2 + h^2 * a_se^2)
      z <- stats::qnorm(1 - (1 - ci_level) / 2)
      df <- .add_inference_cols(df, "headcount_ratio", h,     h_se,   z)
      df <- .add_inference_cols(df, "intensity",       a,     a_se,   z)
      df <- .add_inference_cols(df, "mpi",             h * a, mpi_se, z)
    }
    return(df)
  }

  # By-group: always compute SE; drop afterwards if not requested
  h_by <- as.data.frame(survey::svyby(~ is_deprived, by_f, design, survey::svymean, na.rm = TRUE, keep.names = FALSE, vartype = "se"))
  a_by <- as.data.frame(survey::svyby(~ deprivation_score, by_f, subset(design, is_deprived == 1), survey::svymean, na.rm = TRUE, keep.names = FALSE, vartype = "se"))
  n_by <- as.data.frame(survey::svyby(~ is_deprived, by_f, design, survey::unwtd.count, keep.names = FALSE))

  h_se_orig <- setdiff(names(h_by), c(by_cols, "is_deprived"))[1]
  a_se_orig <- setdiff(names(a_by), c(by_cols, "deprivation_score"))[1]
  names(h_by)[names(h_by) == h_se_orig] <- ".h_se"
  names(a_by)[names(a_by) == a_se_orig] <- ".a_se"
  names(n_by)[names(n_by) == "counts"]  <- "n"

  df <- merge(h_by[, c(by_cols, "is_deprived", ".h_se")], a_by[, c(by_cols, "deprivation_score", ".a_se")], by = by_cols)
  df <- merge(df, n_by[, c(by_cols, "n")], by = by_cols)

  names(df)[names(df) == "is_deprived"]      <- "headcount_ratio"
  names(df)[names(df) == "deprivation_score"] <- "intensity"
  df$mpi <- df$headcount_ratio * df$intensity

  if (inference) {
    h_se   <- df$.h_se
    a_se   <- df$.a_se
    mpi_se <- sqrt(df$intensity^2 * h_se^2 + df$headcount_ratio^2 * a_se^2)
    z      <- stats::qnorm(1 - (1 - ci_level) / 2)
    df$.h_se <- NULL
    df$.a_se <- NULL
    df <- .add_inference_cols(df, "headcount_ratio", df$headcount_ratio, h_se,   z)
    df <- .add_inference_cols(df, "intensity",       df$intensity,       a_se,   z)
    df <- .add_inference_cols(df, "mpi",             df$mpi,             mpi_se, z)
  } else {
    df$.h_se <- NULL
    df$.a_se <- NULL
  }

  first <- c(by_cols, "n", "headcount_ratio", "intensity", "mpi")
  rest  <- setdiff(names(df), first)
  tibble::as_tibble(df[, c(first, rest), drop = FALSE])
}

# Compute weighted indicator headcount ratios.
svy_compute_headcount_ratio <- function(design, by_cols, pattern, inference, ci_level) {
  ind_cols <- grep(pattern, names(design$variables), value = TRUE)
  if (length(ind_cols) == 0) return(tibble::tibble())

  ind_f  <- stats::as.formula(paste("~", paste(paste0("`", ind_cols, "`"), collapse = " + ")))
  has_by <- length(by_cols) > 0
  by_f   <- if (has_by) stats::as.formula(paste("~", paste(by_cols, collapse = " + "))) else NULL

  if (!has_by) {
    res  <- survey::svymean(ind_f, design, na.rm = TRUE)
    ests <- stats::coef(res)
    n    <- nrow(design$variables)
    df   <- tibble::as_tibble(as.data.frame(t(ests)))
    df   <- tibble::add_column(df, n = n, .before = 1)

    if (inference) {
      ses <- as.numeric(survey::SE(res))
      z   <- stats::qnorm(1 - (1 - ci_level) / 2)
      for (j in seq_along(ind_cols)) {
        df <- .add_inference_cols(df, ind_cols[j], ests[j], ses[j], z)
      }
    }
    return(df)
  }

  res  <- as.data.frame(survey::svyby(
    ind_f,
    by_f,
    design,
    survey::svymean,
    na.rm = TRUE,
    keep.names = FALSE,
    vartype = "se")
  )

  n_by <- as.data.frame(survey::svyby(
    stats::as.formula(paste0("~`", ind_cols[1], "`")),
    by_f, design, survey::unwtd.count, keep.names = FALSE
  ))
  names(n_by)[names(n_by) == "counts"] <- "n"

  est_cols <- intersect(ind_cols, names(res))
  df <- tibble::as_tibble(res[, c(by_cols, est_cols), drop = FALSE])
  df <- dplyr::left_join(df, n_by[, c(by_cols, "n")], by = by_cols)
  df <- dplyr::select(df, n, dplyr::everything())

  if (inference) {
    z <- stats::qnorm(1 - (1 - ci_level) / 2)
    for (col in est_cols) {
      se_col <- paste0("se.", col)
      if (se_col %in% names(res)) {
        df <- .add_inference_cols(df, col, res[[col]], res[[se_col]], z)
      }
    }
  }

  tibble::as_tibble(df)
}

# Append _se, _ci_low, _ci_high columns for one estimate, clamped to [0, 1].
.add_inference_cols <- function(df, col, est, se, z) {
  df[[paste0(col, "_se")]]      <- se
  df[[paste0(col, "_ci_low")]]  <- pmax(0, est - z * se)
  df[[paste0(col, "_ci_high")]] <- pmin(1, est + z * se)
  df
}
