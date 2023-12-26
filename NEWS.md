# mpindex 0.2.1

* Changed returned value of `define_mpi_specs` from list to data frame with a class of `mpi_specs_df`. By default, it stores a global option named `mpi_specs` which can be accessed using `getOption('mpi_specs')`. Other specifications are now stored as `attributes`.
* Changed default value of `.unit_of_analysis` from `households` to `NULL`.
* `save_mpi` now allows unformatted output (tidy format) when generating an output. 
* Implemented feature to include MPI specification in the output by setting `.include_specs` tot `TRUE` in `save_mpi`.
* Deprecated `.names_separator` argument in `define_mpi_specs`.
* Enhancement of `define_deprivation` based on #17.
* Added `use_global_mpi_specs()` wrapper function to use Global MPI specification.
