#' Compute MPI from a deprivation profile
#'
#' @description Computes the Multidimensional Poverty Index using a
#' pre-assembled deprivation profile — a named list produced by calling
#' \code{\link[mpindex]{define_deprivation}} once per indicator.
#'
#' This is the lower-level entry point. For a single-call workflow with inline
#' cutoff expressions, use \code{\link[mpindex]{compute_mpi}} with
#' \code{\link[mpindex]{deprived}}.
#'
#' @param .data A tidy data frame where each row is the unit of analysis.
#' @param deprivation_profile A named list of data frames produced by
#'   \code{\link[mpindex]{define_deprivation}}. Names must exactly match the
#'   \code{variable} column in \code{mpi_specs}.
#' @param ... Grouping columns (tidyselect), e.g. \code{region}, \code{sex}.
#' @param mpi_specs MPI specifications from \code{\link[mpindex]{define_mpi_specs}}.
#' @param include_deprivation_matrix Whether to include deprivation matrices
#'   in the output. Default \code{TRUE}.
#' @param generate_output Whether to write an Excel file as a side effect.
#'   Default \code{FALSE}.
#' @param mpi_output_filename Output filename when \code{generate_output = TRUE}.
#' @param include_specs Whether to include MPI specification sheet in Excel output.
#' @param weight Name of the sampling-weight column in \code{.data}. When
#'   supplied, all estimates are survey-weighted. Requires the \pkg{survey}
#'   package.
#' @param strata Name of the stratum column in \code{.data}.
#' @param cluster Name of the cluster / PSU column in \code{.data}.
#' @param fpc Name of the finite-population correction column in \code{.data}.
#' @param survey_design A pre-built \code{survey::svydesign()} object.
#'   Provide either \code{weight} / \code{strata} / \code{cluster} /
#'   \code{fpc} \emph{or} \code{survey_design}, not both.
#' @param inference Logical. When \code{TRUE} (and a survey design is
#'   supplied), standard errors and confidence intervals are appended as
#'   \code{*_se}, \code{*_ci_low}, \code{*_ci_high} columns. Default
#'   \code{FALSE}.
#' @param ci_level Confidence level for intervals. Default \code{0.95}.
#'
#' @return A named list of class \code{mpi_output} with components:
#'   \describe{
#'     \item{\code{$index}}{Named list keyed by \code{k_*}: MPI, H, A, n.}
#'     \item{\code{$contribution}}{Named list keyed by \code{k_*}: contribution by indicator/dimension.}
#'     \item{\code{$headcount_ratio}}{Named list with \code{uncensored} and per-\code{k_*} censored ratios.}
#'     \item{\code{$deprivation_matrix}}{Named list with \code{uncensored} and per-\code{k_*} matrices.}
#'   }
#'
#' @export
#'
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/}{Alkire-Foster Method}
#' @seealso \link[mpindex]{define_mpi_specs}, \link[mpindex]{define_deprivation},
#'   \link[mpindex]{compute_mpi}, \link[mpindex]{save_mpi}
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
  mpi_specs                  = NULL,
  include_deprivation_matrix = TRUE,
  generate_output            = FALSE,
  mpi_output_filename        = NULL,
  include_specs              = FALSE,
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
    c(".deprivation_profile", ".mpi_specs", ".include_deprivation_matrix",
      ".generate_output", ".mpi_output_filename", ".include_specs",
      ".weight", ".strata", ".cluster", ".fpc", ".survey_design",
      ".inference", ".ci_level"),
    ...
  )

  validate_mpi_specs(mpi_specs)

  spec_attr <- attributes(mpi_specs)
  cutoffs   <- spec_attr$poverty_cutoffs
  p_cutoffs <- set_k_label(cutoffs)

  headcount_ratio_list <- list()
  mpi_computed_list    <- list()
  contribution_list    <- list()

  if (!is.null(spec_attr$aggregation)) {
    if (!(spec_attr$aggregation %in% names(.data))) {
      stop("aggregation column defined in specification file does not exist in the dataset.")
    }
  }

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
      mpi_specs = mpi_specs,
      ...
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

  headcount_ratio_list[["uncensored"]] <- compute_headcount_ratio(
    unc_dm,
    aggregation   = spec_attr$aggregation,
    ...,
    survey_design = unc_svy,
    inference     = inference,
    ci_level      = ci_level
  ) |> rename_indicators(mpi_specs = mpi_specs)

  # --- Per-cutoff computation ---------------------------------------------
  for (i in seq_along(p_cutoffs)) {
    dep_label <- set_dep_label(p_cutoffs, i)
    dm_temp   <- deprivation_matrix[[dep_label]]
    dm_svy    <- svy_for_dm(dm_temp)

    incidence_temp <- compute_headcount_ratio(
      dm_temp,
      aggregation   = spec_attr$aggregation,
      ...,
      survey_design = dm_svy,
      inference     = inference,
      ci_level      = ci_level
    )

    headcount_ratio_list[[dep_label]] <- rename_indicators(incidence_temp, mpi_specs = mpi_specs)

    mpi_computed_temp <- compute_headcount_ratio_adjusted(
      dm_temp,
      aggregation   = spec_attr$aggregation,
      ...,
      survey_design = dm_svy,
      inference     = inference,
      ci_level      = ci_level
    )

    mpi_computed_list[[dep_label]] <- rename_n(mpi_computed_temp, spec_attr$unit_of_analysis)

    contribution_list[[dep_label]] <- mpi_computed_temp |>
      dplyr::select(mpi) |>
      dplyr::bind_cols(incidence_temp) |>
      compute_contribution(..., mpi_specs = mpi_specs)
  }

  mpi_output <- list(
    index           = mpi_computed_list,
    contribution    = contribution_list,
    headcount_ratio = headcount_ratio_list
  )

  if (include_deprivation_matrix) {
    dm_n    <- names(deprivation_matrix)
    join_by <- uid_col

    mpi_output[["deprivation_matrix"]] <- stats::setNames(
      lapply(dm_n, function(x) {
        deprivation_matrix[[x]] |>
          dplyr::select(
            !!as.name(join_by),
            dplyr::any_of(spec_attr$aggregation),
            ...,
            dplyr::any_of("deprivation_score"),
            dplyr::matches("^d\\d{2}_i\\d{2}.*")
          ) |>
          rename_indicators(mpi_specs = mpi_specs)
      }),
      dm_n
    )
  }

  if (generate_output) {
    save_mpi(
      mpi_output,
      mpi_specs     = mpi_specs,
      filename      = mpi_output_filename,
      include_specs = include_specs
    )
  }

  class(mpi_output) <- c("mpi_output", class(mpi_output))
  return(mpi_output)
}
