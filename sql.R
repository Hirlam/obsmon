library(pryr)

criterion2clause <- function(name, criteria) {
  val <- criteria[[name]]
  switch(
      name,
      "dtgMin"=sprintf("(%d <= DTG)", val),
      "dtgMax"=sprintf("(DTG < %d)", val),
      "levels"=sprintf("(level in (%s))",
                       do.call(partial(paste, sep=", "), as.list(val))),
      switch(
          typeof(val),
          "integer"=sprintf("(%s = %d)", name, val),
          "character"=sprintf("(%s = '%s')", name, val)
      )
  )
}

buildWhereClause <- function(criteria) {
  subclauses <- list()
  crits <- lapply(names(criteria), partial(criterion2clause, criteria=criteria))
  do.call(partial(paste, sep=" AND "), crits)
}
