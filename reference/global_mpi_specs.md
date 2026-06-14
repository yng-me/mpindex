# Load the built-in Global MPI specification

Returns the `mpi_specs` object for the standard Global MPI (10
indicators across Health, Education, and Living Standards). Assign the
result and pass it explicitly as `mpi_specs`.

## Usage

``` r
global_mpi_specs(..., poverty_cutoffs = 1/3)
```

## Arguments

- ...:

  Additional arguments passed to
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md),
  e.g. `uid`, `poverty_cutoffs`.

- poverty_cutoffs:

  Single value or vector of poverty cutoffs (k). All values must be in
  (0, 1\]. Default is `1/3`.

## Value

An `mpi_specs` object.

## Examples

``` r
mpi_specs <- global_mpi_specs(uid = "uuid")
```
