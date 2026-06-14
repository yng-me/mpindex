#' Compute MPI from a deprivation profile (internal)
#'
#' @description Internal implementation used by \code{\link[mpindex]{compute_mpi}}.
#' Not exported. Use \code{\link[mpindex]{compute_mpi}} directly.
#'
#' @param .data A tidy data frame where each row is the unit of analysis.
#' @param deprivation_profile A named list of data frames produced by
#'   \code{\link[mpindex]{define_deprivation}}.
#' @param ... Extra columns (tidyselect) to carry through into the deprivation
#'   matrix. Not used for grouping.
#' @param by Pre-resolved character vector of grouping column names. Populated
#'   by \code{compute_mpi()} from its \code{by} tidyselect argument.
#' @param mpi_specs MPI specifications from \code{\link[mpindex]{define_mpi_specs}}.
#' @param include_deprivation_matrix Whether to include deprivation matrices.
#' @param weight,strata,cluster,fpc,survey_design,inference,ci_level Survey
#'   design arguments; forwarded from \code{\link[mpindex]{compute_mpi}}.
#'
#' @keywords internal
#'
#' @examples
#' specs <- define_mpi_specs(
#'   system.file("extdata", "global-mpi-specs.csv", package = "mpindex"),
#'   uid = "uuid"
#' )
#'
#' deprivation_profile <- list()
#'
#' deprivation_profile$drinking_water <- df_household |>
#'   define_deprivation(
#'     indicator = drinking_water,
#'     cutoff    = drinking_water == 2,
#'     mpi_specs = specs
#'   )
#'
#' # ... (define remaining indicators) ...
#'
#' \dontrun{
#' mpi_result <- compute_mpi_from_profile(
#'   df_household, deprivation_profile, mpi_specs = specs
#' )
#' }
#'
compute_mpi_from_profile <- function(
  .data,
  deprivation_profile,
  ...,
  by                         = character(0),
  mpi_specs                  = NULL,
  include_deprivation_matrix = TRUE,
  weight                     = NULL,
  strata                     = NULL,
  cluster                    = NULL,
  fpc                        = NULL,
  survey_design              = NULL,
  inference                  = FALSE,
  ci_level                   = 0.95
) {

  check_old_dotted_args(
    "compute_mpi_from_profile",
    c(".deprivation_profile", ".mpi_specs", ".include_deprivation_matrix"),
    ...
  )

  validate_mpi_specs(mpi_specs)

  spec_attr <- attributes(mpi_specs)
  cutoffs   <- spec_attr$poverty_cutoffs
  p_cutoffs <- set_k_label(cutoffs)

  headcount_ratio_list <- list()
  mpi_computed_list    <- list()
  contribution_list    <- list()

  by_cols <- by

  if ("mpi_dm" %in% class(.data)) {
    deprivation_matrix <- .data
  } else {
    if (is.null(deprivation_profile) ||
        !(identical(sort(mpi_specs$variable), sort(names(deprivation_profile))))) {
      stop("Deprivation profile is incomplete.")
    }
    deprivation_matrix <- create_deprivation_matrix(
      .data,
      deprivation_profile,
      ...,
      by_cols   = by_cols,
      mpi_specs = mpi_specs
    )
  }

  # --- Build survey design (if requested) ---------------------------------
  uid_col <- if (!is.null(spec_attr$uid)) as.character(spec_attr$uid) else "uid"
  svy     <- resolve_survey_design(.data, weight, strata, cluster, fpc, survey_design)

  svy_for_dm <- function(dm_slice) {
    if (is.null(svy)) return(NULL)
    design_with_dm(svy, dm_slice, uid_col)
  }

  # --- Uncensored headcount ratio -----------------------------------------
  unc_dm  <- deprivation_matrix$uncensored
  unc_svy <- svy_for_dm(unc_dm)

  headcount_ratio_list[["uncensored"]] <- rlang::inject(
    compute_headcount_ratio(
      unc_dm,
      !!!rlang::syms(by_cols),
      survey_design = unc_svy,
      inference     = inference,
      ci_level      = ci_level
    )
  ) |> rename_indicators(mpi_specs = mpi_specs)

  # --- Per-cutoff computation ---------------------------------------------
  for (i in seq_along(p_cutoffs)) {
    dep_label <- set_dep_label(p_cutoffs, i)
    dm_temp   <- deprivation_matrix[[dep_label]]
    dm_svy    <- svy_for_dm(dm_temp)

    incidence_temp <- rlang::inject(
      compute_headcount_ratio(
        dm_temp,
        !!!rlang::syms(by_cols),
        survey_design = dm_svy,
        inference     = inference,
        ci_level      = ci_level
      )
    )

    headcount_ratio_list[[dep_label]] <- rename_indicators(incidence_temp, mpi_specs = mpi_specs)

    mpi_computed_temp <- rlang::inject(
      compute_headcount_ratio_adjusted(
        dm_temp,
        !!!rlang::syms(by_cols),
        survey_design = dm_svy,
        inference     = inference,
        ci_level      = ci_level
      )
    )

    mpi_computed_list[[dep_label]] <- rename_n(mpi_computed_temp, spec_attr$unit_of_analysis)

    tmp <- mpi_computed_temp |>
      dplyr::select(mpi) |>
      dplyr::bind_cols(incidence_temp)

    contribution_list[[dep_label]] <- rlang::inject(
      compute_contribution(tmp, !!!rlang::syms(by_cols), mpi_specs = mpi_specs)
    )
  }

  class(mpi_computed_list) <- c("mpi_list", class(mpi_computed_list))
  class(headcount_ratio_list) <- c("mpi_hr_list", class(headcount_ratio_list))
  class(contribution_list) <- c("mpi_c_list", class(contribution_list))

  mpi_output <- list(
    index           = mpi_computed_list,
    headcount_ratio = headcount_ratio_list,
    contribution    = contribution_list
  )

  if (include_deprivation_matrix) {
    dm_n    <- names(deprivation_matrix)
    join_by <- uid_col

    mpi_output[["deprivation_matrix"]] <- stats::setNames(
      lapply(dm_n, function(x) {
        deprivation_matrix[[x]] |>
          dplyr::select(
            !!as.name(join_by),
            dplyr::any_of(by_cols),
            ...,
            dplyr::any_of("deprivation_score"),
            dplyr::matches("^d\\d{2}_i\\d{2}.*")
          ) |>
          rename_indicators(mpi_specs = mpi_specs)
      }),
      dm_n
    )
  }

  if (length(by_cols) > 0) {
    overall_hr_list  <- list()
    overall_mpi_list <- list()
    overall_ct_list  <- list()

    overall_hr_list[["uncensored"]] <- compute_headcount_ratio(
      unc_dm,
      survey_design = unc_svy,
      inference     = inference,
      ci_level      = ci_level
    ) |> rename_indicators(mpi_specs = mpi_specs)

    for (i in seq_along(p_cutoffs)) {
      dep_label <- set_dep_label(p_cutoffs, i)
      dm_temp   <- deprivation_matrix[[dep_label]]
      dm_svy    <- svy_for_dm(dm_temp)

      inc_temp <- compute_headcount_ratio(
        dm_temp,
        survey_design = dm_svy,
        inference     = inference,
        ci_level      = ci_level
      )
      overall_hr_list[[dep_label]] <- rename_indicators(inc_temp, mpi_specs = mpi_specs)

      mpi_temp <- compute_headcount_ratio_adjusted(
        dm_temp,
        survey_design = dm_svy,
        inference     = inference,
        ci_level      = ci_level
      )
      overall_mpi_list[[dep_label]] <- rename_n(mpi_temp, spec_attr$unit_of_analysis)

      tmp_overall <- mpi_temp |>
        dplyr::select(mpi) |>
        dplyr::bind_cols(inc_temp)
      overall_ct_list[[dep_label]] <- compute_contribution(
        tmp_overall, mpi_specs = mpi_specs
      )
    }

    mpi_output[["overall"]] <- list(
      index           = overall_mpi_list,
      headcount_ratio = overall_hr_list,
      contribution    = overall_ct_list
    )
  }

  attr(mpi_output, "grouping") <- by_cols
  class(mpi_output) <- c("mpi_output", class(mpi_output))
  return(mpi_output)
}
