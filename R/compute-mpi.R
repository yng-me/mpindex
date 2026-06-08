#' Compute Multidimensional Poverty Index (MPI)
#'
#' @description The primary single-call API for computing the MPI using the
#' Alkire-Foster (AF) counting method. Deprivation cutoffs are specified inline
#' using the \code{\link[mpindex]{deprived}} helper, making the workflow
#' self-contained and readable.
#'
#' For a step-by-step workflow using a pre-assembled deprivation profile, see
#' \code{\link[mpindex]{compute_mpi_from_profile}}.
#'
#' @param .data A data frame where each row is the unit of analysis.
#' @param mpi_specs MPI specifications from \code{\link[mpindex]{define_mpi_specs}}.
#' @param deprivations A named list of \code{\link[mpindex]{deprived}} calls. Each
#'   name must exactly match a \code{variable} in \code{mpi_specs}.
#' @param by \emph{(Optional)} Columns to group results by, passed as a
#'   tidyselect expression, e.g. \code{c(region, sex)}.
#' @param include_deprivation_matrix Whether to include deprivation matrices.
#'   Default \code{TRUE}.
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
#' @param ... Grouping columns (tidyselect) or reserved for old-name detection.
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
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/}{Alkire-Foster Method} \cr
#' \href{https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/}{How to Apply the Alkire-Foster Method}
#' @seealso \link[mpindex]{define_mpi_specs}, \link[mpindex]{deprived},
#'   \link[mpindex]{compute_mpi_from_profile}, \link[mpindex]{save_mpi}
#'
#' @examples
#' specs <- define_mpi_specs(
#'   system.file("extdata", "global-mpi-specs.csv", package = "mpindex"),
#'   uid = "uuid"
#' )
#'
#' \dontrun{
#' mpi_result <- compute_mpi(
#'   df_household,
#'   mpi_specs = specs,
#'   deprivations = list(
#'     nutrition = deprived(
#'       undernourished == 1 & age < 70,
#'       .data = df_household_roster,
#'       collapse_fn = max
#'     ),
#'     child_mortality = deprived(with_child_died == 1),
#'     year_schooling = deprived(
#'       completed_6yrs_schooling == 2,
#'       .data = df_household_roster,
#'       collapse_fn = max
#'     ),
#'     school_attendance = deprived(
#'       attending_school == 2 & age %in% c(5:24),
#'       .data = df_household_roster,
#'       collapse_fn = max
#'     ),
#'     cooking_fuel   = deprived(cooking_fuel %in% c(4:6, 9)),
#'     sanitation     = deprived(toilet > 1),
#'     drinking_water = deprived(drinking_water == 2),
#'     electricity    = deprived(electricity == 2),
#'     housing        = deprived(
#'       roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
#'     ),
#'     assets = deprived(!(
#'       (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
#'          asset_animal_cart + asset_bicycle + asset_motorcycle +
#'          asset_refrigerator) > 1 &
#'         (asset_car + asset_truck) > 0
#'     ))
#'   )
#' )
#' }
#'
compute_mpi <- function(
  .data,
  mpi_specs,
  deprivations,
  ...,
  by                         = NULL,
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
    "compute_mpi",
    c(".mpi_specs", ".deprivations", ".by", ".include_deprivation_matrix",
      ".generate_output", ".mpi_output_filename", ".include_specs",
      ".weight", ".strata", ".cluster", ".fpc", ".survey_design",
      ".inference", ".ci_level"),
    ...
  )

  validate_mpi_specs(mpi_specs)

  # Resolve `by` into a character vector of column names
  by_cols <- names(dplyr::select(.data, {{by}}))

  # Validate deprivations names
  dep_names <- names(deprivations)
  if (is.null(dep_names) || any(dep_names == "")) {
    stop("All elements of `deprivations` must be named.")
  }
  if (any(duplicated(dep_names))) {
    stop("`deprivations` contains duplicate names.")
  }
  extra <- setdiff(dep_names, mpi_specs$variable)
  if (length(extra) > 0) {
    stop(paste0(
      "Names in `deprivations` not found in specs: ",
      paste(extra, collapse = ", ")
    ))
  }

  # Build deprivation profile
  deprivation_profile <- vector("list", length(dep_names))
  names(deprivation_profile) <- dep_names

  for (ind in dep_names) {
    entry <- deprivations[[ind]]

    if (inherits(entry, "mpi_deprivation_spec")) {
      data_for_ind <- if (!is.null(entry$data)) entry$data else .data

      deprivation_profile[[ind]] <- rlang::inject(
        define_deprivation(
          .data           = data_for_ind,
          indicator       = !!rlang::sym(ind),
          cutoff          = !!entry$cutoff,
          mpi_specs       = mpi_specs,
          collapse_fn     = entry$collapse_fn,
          set_na_equal_to = entry$set_na_equal_to
        )
      )
    } else if (is.data.frame(entry)) {
      deprivation_profile[[ind]] <- entry
    } else {
      stop(paste0("Element '", ind, "' must be a deprived() spec or a data frame."))
    }
  }

  # Delegate to compute_mpi_from_profile, injecting `by` columns as ...
  rlang::inject(
    compute_mpi_from_profile(
      .data,
      deprivation_profile,
      ...,
      !!!rlang::syms(by_cols),
      mpi_specs                  = mpi_specs,
      include_deprivation_matrix = include_deprivation_matrix,
      generate_output            = generate_output,
      mpi_output_filename        = mpi_output_filename,
      include_specs              = include_specs,
      weight                     = weight,
      strata                     = strata,
      cluster                    = cluster,
      fpc                        = fpc,
      survey_design              = survey_design,
      inference                  = inference,
      ci_level                   = ci_level
    )
  )
}
