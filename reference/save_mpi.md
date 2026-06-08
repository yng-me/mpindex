# Save MPI output

Save the MPI output to an Excel file using the
[tsg](https://cran.r-project.org/package=tsg) package for
publication-ready table formatting.

## Usage

``` r
save_mpi(
  mpi_output,
  mpi_specs = NULL,
  filename = NULL,
  include_deprivation_matrix = TRUE,
  include_specs = FALSE,
  ...
)
```

## Arguments

- mpi_output:

  An object derived from
  [compute_mpi](https://yng-me.github.io/mpindex/reference/compute_mpi.md).

- mpi_specs:

  MPI specifications defined in
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md).

- filename:

  Output filename. The `.xlsx` extension is added automatically when
  missing. Defaults to `"MPI Results.xlsx"` in the current working
  directory.

- include_deprivation_matrix:

  Whether to include deprivation matrices as separate sheets. Defaults
  to `TRUE`.

- include_specs:

  Whether to include MPI specification as a separate sheet. Defaults to
  `FALSE`.

- ...:

  Reserved; passing old dotted names triggers a helpful error.

## Value

Returns the normalised file path of the generated Excel file.

## Examples

``` r
if (FALSE) { # \dontrun{
mpi_result <- compute_mpi(df_household, mpi_specs = specs, deprivations = deps)
save_mpi(mpi_result, mpi_specs = specs, filename = "MPI Sample Output")
} # }
```
