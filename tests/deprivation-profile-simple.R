library(mpindex)

indicators <- data.frame(
  dimension = c("A", "B"),
  indicator = c("A1", "A2", "B1", "B2"),
  variable = c("a1", "a2", "b1", "b2"),
  weight = rep(1 / 4, 4),
  variable_name = c("d01_i01_a1", "d01_i02_a2", "d02_i01_b1", "d02_i02_b2"),
  label = c("A>A1", "A>A2", "B>B1", "B>B2")
) |> tibble::tibble()

specs_simple <- define_mpi_specs(
  .poverty_cutoffs = 1 / 2,
  .indicators = indicators
)

df_simple <- data.frame(
  a1 = c(0, 1, 1, 0),
  a2 = c(0, 0, 1, 1),
  b1 = c(0, 0, 1, 0),
  b2 = c(0, 1, 1, 0)
) |> tibble::tibble()

dp_simple <- list()
dp_simple$a1 <- df_simple |>
  define_deprivation(a1, a1 == 1, .mpi_specs = specs_simple)
dp_simple$a2 <- df_simple |>
  define_deprivation(a2, a2 == 1, .mpi_specs = specs_simple)
dp_simple$b1 <- df_simple |>
  define_deprivation(b1, b1 == 1, .mpi_specs = specs_simple)
dp_simple$b2 <- df_simple |>
  define_deprivation(b2, b2 == 1, .mpi_specs = specs_simple)
