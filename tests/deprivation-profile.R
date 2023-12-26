library(mpindex)

mpi_specs <- use_global_mpi_specs(.uid = "uuid")
attr_spec <- attributes(mpi_specs)
deprivation_profile <- list()

deprivation_profile$nutrition <- df_household_roster |>
  define_deprivation(
    .indicator = nutrition,
    .cutoff = undernourished == 1 & age < 70,
    .collapse = TRUE,
    .mpi_specs = mpi_specs
  )

deprivation_profile$child_mortality <- df_household |>
  define_deprivation(
    .indicator = child_mortality,
    .cutoff = with_child_died == 1,
    .mpi_specs = mpi_specs
  )

deprivation_profile$year_schooling <- df_household_roster |>
  define_deprivation(
    .indicator = year_schooling,
    .cutoff = completed_6yrs_schooling == 2,
    .collapse = TRUE,
    .mpi_specs = mpi_specs
  )

deprivation_profile$school_attendance <- df_household_roster |>
  define_deprivation(
    .indicator = school_attendance,
    .cutoff = attending_school == 2 & age %in% c(5:24),
    .collapse = TRUE,
    .mpi_specs = mpi_specs
  )

deprivation_profile$cooking_fuel <- df_household |>
  define_deprivation(
    .indicator = cooking_fuel,
    .cutoff = cooking_fuel %in% c(4:6, 9),
    .mpi_specs = mpi_specs
  )

deprivation_profile$sanitation <- df_household |>
  define_deprivation(
    .indicator = sanitation,
    .cutoff = toilet > 1,
    .mpi_specs = mpi_specs
  )

deprivation_profile$drinking_water <- df_household |>
  define_deprivation(
    .indicator = drinking_water,
    .cutoff = drinking_water == 2,
    .mpi_specs = mpi_specs
  )

deprivation_profile$electricity <- df_household |>
  define_deprivation(
    .indicator = electricity,
    .cutoff = electricity == 2,
    .mpi_specs = mpi_specs
  )

deprivation_profile$housing <- df_household |>
  define_deprivation(
    .indicator = housing,
    .cutoff = roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9),
    .mpi_specs = mpi_specs
  )


deprivation_profile$assets <- df_household |>
  dplyr::mutate_at(
    dplyr::vars(dplyr::starts_with("asset_")),
    ~ dplyr::if_else(. > 0, 1L, 0L)
  ) |>
  dplyr::mutate(
    asset_phone = dplyr::if_else(
      (asset_telephone + asset_mobile_phone) > 0,
      1L,
      0L
    )
  ) |>
  dplyr::mutate(
    with_hh_conveniences = (asset_tv + asset_phone + asset_computer +
      asset_animal_cart + asset_bicycle +
      asset_motorcycle + asset_refrigerator) > 1,
    with_mobility_assets = (asset_car + asset_truck) > 0
  ) |>
  define_deprivation(
    .indicator = assets,
    .cutoff = !(with_hh_conveniences & with_mobility_assets),
    .mpi_specs = mpi_specs
  )
