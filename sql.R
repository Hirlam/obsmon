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

notValidCrit <- function(crit) {
  rtn <- is.null(crit) || is.na(crit) || length(crit)==0 || crit==''
  return(rtn)
}

buildWhereClause <- function(criteria) {
  subclauses <- list()
  criteriaNames <- names(criteria)
  crits <- lapply(criteriaNames[criteriaNames!="info"],
                  partial(criterion2clause, criteria=criteria))
  crits <- crits[!(is.null(crits))]
  # Removing NULL, NA or empty values from the crits list.
  # This avoids, e.g., building invalid queries with dangling ANDs
  # (e.g., "WHERE AND", "AND AND", etc)
  crits[sapply(crits, notValidCrit)] <- NULL
  do.call(partial(paste, sep=" AND "), crits)
}
