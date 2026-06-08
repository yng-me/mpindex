# Survey test fixtures
# Adds synthetic sampling columns to df_household and builds standard designs.

set.seed(2025)
n_hh <- nrow(df_household)

df_household_svy <- df_household
df_household_svy$hh_weight <- runif(n_hh, 0.8, 2.5)
df_household_svy$strata    <- sample(c("urban", "rural"), n_hh, replace = TRUE)
df_household_svy$psu       <- sample(1:30, n_hh, replace = TRUE)

svy_design_prebuilt <- survey::svydesign(
  ids     = ~psu,
  strata  = ~strata,
  weights = ~hh_weight,
  nest    = TRUE,
  data    = df_household_svy
)

# Full deprivation spec used in all survey tests
svy_deps <- list(
  nutrition         = deprived(undernourished == 1 & age < 70,
                               .data = df_household_roster, collapse_fn = max),
  child_mortality   = deprived(with_child_died == 1),
  year_schooling    = deprived(completed_6yrs_schooling == 2,
                               .data = df_household_roster, collapse_fn = max),
  school_attendance = deprived(attending_school == 2 & age %in% 5:24,
                               .data = df_household_roster, collapse_fn = max),
  cooking_fuel      = deprived(cooking_fuel %in% c(4:6, 9)),
  sanitation        = deprived(toilet > 1),
  drinking_water    = deprived(drinking_water == 2),
  electricity       = deprived(electricity == 2),
  housing           = deprived(
    roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
  ),
  assets = deprived(!(
    (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
       asset_animal_cart + asset_bicycle + asset_motorcycle +
       asset_refrigerator) > 1 &
      (asset_car + asset_truck) > 0
  ))
)
