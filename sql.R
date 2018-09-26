dtgClause <- function(val) {
  n = length(val)
  if (n==1) {
    sprintf("(DTG = %d)", val)
  } else if (n==3) {
    range <- expandDateRange(val)
    sprintf("(%d <= DTG) AND (DTG <= %d) AND (DTG %% 100 IN (%s))",
            range[[1]], range[[2]], paste(range[[3]], collapse=", "))
  } else {
    NULL
  }
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
  criteriaNames <- names(criteria)
  crits <- lapply(criteriaNames[criteriaNames!="info"],
                  partial(criterion2clause, criteria=criteria))
  do.call(partial(paste, sep=" AND "), crits)
}
