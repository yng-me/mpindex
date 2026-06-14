utils::globalVariables(
  c(
    ".",
    ":=",
    "cutoff",
    "deprivation_score",
    "dimension",
    "headcount_ratio",
    "indicator",
    "intensity",
    "is_deprived",
    "label",
    "m",
    "mpi",
    "n",
    "uid",
    "value",
    "variable",
    "variable_name",
    "weight",
    "poverty_cutoff"
  )
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mpindex <- list(
    mpindex.options = list(
      names_separator = "__"
    )
  )

  to_set <- !(names(op.mpindex) %in% names(op))
  if (any(to_set)) options(op.mpindex[to_set])

  invisible()
}
