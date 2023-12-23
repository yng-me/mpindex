# mpindex 0.2.1

* Changed returned value of `define_mpi_specs` from list to data frame with a class of `mpi_specs_df`. By default, it stores a global option named `mpi_specs` which can be accessed using `getOption('mpi_specs')`. Other specifications are now stored as `attributes`.
* Deprecated `.names_separator` argument in `define_mpi_specs`.
* Added `use_global_mpi_specs()` wrapper function to use Global MPI specification.
