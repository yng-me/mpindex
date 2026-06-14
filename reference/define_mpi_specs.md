# Define MPI specifications: dimensions, indicators, and weights

Use to define MPI dimensions, indicators and its corresponding weights
using any of the supported file types: `.xlsx` (Excel), `.json`, `.csv`,
or `.txt` (TSV). You can also set the poverty cutoff or list of poverty
cutoffs.

## Usage

``` r
define_mpi_specs(
  mpi_specs_file = NULL,
  indicators = NULL,
  poverty_cutoffs = NULL,
  unit_of_analysis = NULL,
  uid = NULL,
  source_of_data = NULL,
  names_separator = getOption("mpindex.options")$names_separator %||% "__",
  save_as_global_options = FALSE,
  ...
)
```

## Arguments

- mpi_specs_file:

  Path to a `.xlsx`, `.json`, `.csv`, or `.txt` (TSV) file. The file
  must contain columns: `Dimension`, `Indicator`, `Variable`, `Weight`,
  and optionally `Description`.

- indicators:

  A data frame of MPI indicators. Alternative to `mpi_specs_file` when
  you prefer to define indicators inline.

- poverty_cutoffs:

  Single value or vector of poverty cutoffs (k). All values must be in
  (0, 1\]. Default is `NULL` which will be automatically set to `1/n`,
  where `n` is the total number of dimensions.

- unit_of_analysis:

  e.g. `"individuals"`, `"households"`. Default `NULL`.

- uid:

  Column name containing the unique ID (unit of analysis).

- source_of_data:

  Source of data; used in output footnotes.

- names_separator:

  **\[deprecated\]** Column separator for the header hierarchy.

- save_as_global_options:

  **\[deprecated\]** No longer has any effect.

- ...:

  Reserved for forward-compatibility; passing old dotted argument names
  (e.g. `.uid`) triggers a helpful error.

## Value

An `mpi_specs` object. Pass this directly as the `mpi_specs` argument in
[`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md),
[`define_deprivation`](https://yng-me.github.io/mpindex/reference/define_deprivation.md),
and
[`save_mpi`](https://yng-me.github.io/mpindex/reference/save_mpi.md).

## See also

[compute_mpi](https://yng-me.github.io/mpindex/reference/compute_mpi.md)

## Examples

``` r
specs_file <- system.file(
  "extdata",
  "global-mpi-specs.csv",
  package = "mpindex"
)
system.file("extdata", package = "mpindex") |> list.files()
#> [1] "global-mpi-specs.csv"  "global-mpi-specs.json" "global-mpi-specs.txt" 
#> [4] "global-mpi-specs.xlsx"
```
