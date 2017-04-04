library(pryr)

dtgClause <- function(val) {
  switch(length(val),
         sprintf("(DTG = %d)", val),
         sprintf("(%d <= DTG) AND (DTG <= %d)", val[[1]], val[[2]]))
}

criterion2clause <- function(name, criteria) {
  val <- criteria[[name]]
  switch(
      name,
      "dtg"=dtgClause(val),
      "levels"=sprintf("(level in (%s))",
                       do.call(partial(paste, sep=", "), as.list(val))),
      "station"=sprintf("(statid like '%%%s%%')", val),
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
