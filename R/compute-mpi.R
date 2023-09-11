#' Compute Multidimensional Poverty Index (MPI)
#'
#' @description This function uses the Alkire-Foster (AF) counting method developed by Sabina Alkire and James Foster. It requires a deprivation profile created using the (\code{\link[mpindex]{define_deprivation}}) fuction containing all indicators defined in the specification files.
#'
#' @param .data A tidy data frame where each observation is the unit of analysis defined in \code{\link[mpindex]{define_mpi_specs}}.
#' @param .deprivation_profile list of deprivation profile created using \code{\link[mpindex]{define_deprivation}}.
#' @param ... Grouping columns (supports tidyselect), e.g. area (country, urbanity, region, province), sex, ethnic group, etc.
#' @param .mpi_specs MPI specifications defined in \code{\link[mpindex]{define_mpi_specs}}.
#' @param .include_deprivation_matrix Whether to include deprivation matrix in the output.
#' @param .generate_output Whether to generate an output (Excel file) as side effect.
#' @param .output_filename Output filename.
#' @param .formatted_output NOT YET IMPLEMENTED. Whether formatting is to be applied to the output.
#' @param .include_table_summary NOT YET IMPLEMENTED. Whether to include summary information in the generated output.
#' @param .include_specs NOT YET IMPLEMENTED. Whether to include MPI specification in the generated output.
#'
#' @return Returns list of objects: \code{index} (the MPI), \code{contribution} (contribution by dimension), \code{headcount_ratio} (censored and uncensored), and \code{deprivation_matrix} (censored and uncensored). If \code{poverty_cutoffs} defined in \code{\link[mpindex]{define_mpi_specs}} contain more than one (1) value, \code{index} and \code{contribution} object will output each cutoff in a separate table.
#'
#' @export
#'
#' @references \href{https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/}{Alkire-Foster Method} \cr
#' \href{https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/}{How to Apply the Alkire-Foster Method}
#' @seealso \link[mpindex]{define_mpi_specs}, \link[mpindex]{define_deprivation}, \link[mpindex]{save_mpi}
#' @examples
#' # ----------------------------------
#' # Load MPI specs from the built-in specs file
#' specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
#' mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid')
#' options(mpi_specs = mpi_specs)
#'
#' # ----------------------------------
#' # Create an empty list to store deprivation profile for each indicator
#' deprivation_profile <- list()
#'
#' deprivation_profile$nutrition <- df_household_roster |>
#'  define_deprivation(
#'    .indicator = nutrition,
#'    .cutoff = undernourished == 1 & age < 70,
#'    .collapse = TRUE
#'  )
#' deprivation_profile$child_mortality <- df_household |>
#'  define_deprivation(
#'    .indicator = child_mortality,
#'    .cutoff = with_child_died == 1
#'  )
#' deprivation_profile$year_schooling <- df_household_roster |>
#'  define_deprivation(
#'    .indicator = year_schooling,
#'    .cutoff = completed_6yrs_schooling == 2,
#'    .collapse = TRUE
#'  )
#' deprivation_profile$school_attendance <- df_household_roster |>
#'  define_deprivation(
#'    .indicator = school_attendance,
#'    .cutoff = attending_school == 2 & age %in% c(5:24),
#'    .collapse = TRUE
#'  )
#' deprivation_profile$cooking_fuel <- df_household |>
#'  define_deprivation(
#'    .indicator = cooking_fuel,
#'    .cutoff = cooking_fuel %in% c(4:6, 9)
#'  )
#' deprivation_profile$sanitation <- df_household |>
#'  define_deprivation(
#'    .indicator = sanitation,
#'    .cutoff = toilet > 1
#'  )
#' deprivation_profile$drinking_water <- df_household |>
#'  define_deprivation(
#'    .indicator = drinking_water,
#'    .cutoff = drinking_water == 2
#'  )
#' deprivation_profile$electricity <- df_household |>
#'  define_deprivation(
#'    .indicator = electricity,
#'    .cutoff = electricity == 2
#'  )
#' deprivation_profile$housing <- df_household |>
#'  define_deprivation(
#'    .indicator = housing,
#'    .cutoff = roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
#'  )
#' deprivation_profile$assets <- df_household |>
#'  dplyr::mutate_at(dplyr::vars(dplyr::starts_with('asset_')), ~ dplyr::if_else(. > 0, 1L, 0L)) |>
#'  dplyr::mutate(
#'    asset_phone = dplyr::if_else(
#'      (asset_telephone + asset_mobile_phone) > 0,
#'      1L,
#'      0L
#'    )
#'  ) |>
#'  dplyr::mutate(
#'    with_hh_conveniences = (
#'      asset_tv + asset_phone + asset_computer +
#'        asset_animal_cart + asset_bicycle +
#'        asset_motorcycle + asset_refrigerator) > 1,
#'    with_mobility_assets = (asset_car + asset_truck) > 0
#'  ) |>
#'  define_deprivation(
#'    .indicator = assets,
#'    .cutoff = !(with_hh_conveniences & with_mobility_assets)
#'  )
#'
#' # ----------------------------------
#' # Compute the MPI
#' mpi_result <- df_household |>
#'   compute_mpi(deprivation_profile)
#'
#' # ----------------------------------
#' # You may also save your output into an Excel file
#' \dontrun{
#' save_mpi(mpi_result, .filename = 'MPI Sample Output')
#' }

compute_mpi <- function(
  .data,
  .deprivation_profile,
  ...,
  .mpi_specs = getOption('mpi_specs'),
  .include_deprivation_matrix = TRUE,
  .generate_output = FALSE,
  .formatted_output = TRUE,
  .output_filename = NULL,
  .include_table_summary = TRUE,
  .include_specs = FALSE
) {

  n <- NULL
  H <- NULL
  A <- NULL
  MPI <- NULL
  is_deprived <- NULL
  deprivation_score <- NULL

  .cutoffs <- .mpi_specs$poverty_cutoffs
  .p_cutoffs <- set_k_label(.cutoffs)

  .headcount_ratio_list <- list()
  .mpi_computed_list <- list()
  .contribution_list <- list()

  if(is.null(.mpi_specs)) {
    stop('MPI specifications must be defined first.')
  }

  if(!is.null(.mpi_specs$aggregation)) {
    if(!(.mpi_specs$aggregation %in% names(.data))) {
      stop('aggregation column defined in specification file does not exist in the dataset.')
    }
  }


  if('mpi_dep_matrix' %in% class(.data)) {

    .deprivation_profile <- NULL
    .deprivation_matrix <- .data

  } else {

    if(!(identical(sort(.mpi_specs$indicators$variable), sort(names(.deprivation_profile))))) {
      stop('Deprivation profile is incomplete.')
    }

    .deprivation_matrix <- .data |>
      create_deprivation_matrix(
        ...,
        .deprivation_profile,
        .mpi_specs = .mpi_specs
      )

  }

  # Incidence of Poverty -------------------------------------------------------
  .headcount_ratio_list[['uncensored']] <- .deprivation_matrix$uncensored |>
    compute_headcount_ratio(.aggregation = .mpi_specs$aggregation, ...) |>
    rename_indicators(.mpi_specs = .mpi_specs)

  for(i in seq_along(.p_cutoffs)) {

    .dep_label <- set_dep_label(.p_cutoffs, i)
    .dm_temp <- .deprivation_matrix[[.dep_label]]

    .incidence_temp <- .dm_temp |>
      compute_headcount_ratio(.aggregation = .mpi_specs$aggregation, ...)

    .headcount_ratio_list[[.dep_label]] <- .incidence_temp |>
      rename_indicators(.mpi_specs = .mpi_specs)

    .mpi_computed_temp <- .dm_temp |>
      compute_headcount_ratio_adj(.aggregation = .mpi_specs$aggregation, ...)

    .mpi_computed_list[[.dep_label]] <- .mpi_computed_temp |>
      dplyr::rename(
        'Headcount Ratio (H)' = H,
        'Intensity of Deprivation Among the Poor (A)' = A,
        'MPI (H x A)' = MPI
      ) |>
      rename_n(.mpi_specs$unit_of_analysis)

    .contribution_list[[.dep_label]] <- .mpi_computed_temp |>
      dplyr::select(MPI) |>
      dplyr::bind_cols(.incidence_temp) |>
      compute_contribution(..., .mpi_specs = .mpi_specs)
  }


  if(length(.p_cutoffs) == 1) {
    .mpi_computed_list <- .mpi_computed_list[[1]]
    .contribution_list <- .contribution_list[[1]]
  }

  .output <- list(
    index = .mpi_computed_list,
    contribution = .contribution_list,
    headcount_ratio = .headcount_ratio_list
  )

  if(.include_deprivation_matrix) {
    .dm_n <- names(.deprivation_matrix)

    if(!is.null(.mpi_specs$uid)) {
      join_by <- .mpi_specs$uid
    } else {
      join_by <- 'uid'
    }

    .output[['deprivation_matrix']] <- stats::setNames(
      lapply(.dm_n, function(x) {
        .deprivation_matrix[[x]] |>
          dplyr::select(
            !!as.name(join_by),
            dplyr::any_of(.mpi_specs$aggregation),
            ...,
            dplyr::any_of('deprivation_score'),
            dplyr::matches('^d\\d{2}_i\\d{2}.*')
          ) |>
          rename_indicators(.mpi_specs = .mpi_specs)
      }),
    .dm_n
    )
  }

  if(.generate_output) {
    save_mpi(
      .output,
      .mpi_specs = .mpi_specs,
      .formatted_output = .formatted_output,
      .filename = .output_filename,
      .include_table_summary = .include_table_summary,
      .include_specs = .include_specs
    )
  }

  return(.output)
}



