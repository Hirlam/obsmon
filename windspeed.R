buildFfData <- function(db, plotter, plotRequest) {
  varname <- plotRequest$criteria$varname
  suffix <- substr(varname, 3, nchar(varname))
  uName <- paste0("u", suffix)
  vName <- paste0("v", suffix)
  ffName <- paste0("ff", suffix)
  plotRequest$criteria$varname <- uName
  uQuery <- plotBuildQuery(plotter, plotRequest)
  uData <- performQuery(db, uQuery, plotRequest$criteria$dtg)
  if (is.null(uData) || length(uData)==0) {
    return(NULL)
  }
  plotRequest$criteria$varname <- vName
  vQuery <- plotBuildQuery(plotter, plotRequest)
  vData <- performQuery(db, vQuery, plotRequest$criteria$dtg)
  tableString <- regmatches(uQuery, regexpr("FROM \\w* WHERE",
                                            uQuery, ignore.case=TRUE))
  table <- substr(tableString, 6, nchar(tableString)-6)
  switch(tolower(table),
         "obsmon"={
           ffData <- buildFfDataObsmon(plotter, uData, vData, ffName)
         },
         "usage"={
           ffData <- buildFfDataUsage(plotter, uData, vData, ffName)
         })
  ffData
}

buildFfDataObsmon <- function(plotter, uData, vData, ffName) {
  buildColumn <- function(col) {
    switch(
        col,
        "DTG"=,
        "level"=,
        "nobs_total"=uData[col],
        "varname"=ffName,
        NULL
    )
  }
  colNames <- colnames(uData)
  ffData <- buildColumn(colNames[1])
  for (colName in colNames[2:length(colNames)]) {
    col <- buildColumn(colName)
    if (is.null(col)) {
      return(NULL)
    }
    ffData[colName] <- col
  }
  ffData
}

buildFfDataUsage <- function(plotter, uData, vData, ffName) {
  buildPlotValues <- function(dataColumn) {
    switch(dataColumn,
           "fg_dep"=,
           "an_dep"={
             uFgAn <- uData$obsvalue-uData$plotValues
             vFgAn <- vData$obsvalue-vData$plotValues
             fFgAn <- sqrt(uFgAn^2 + vFgAn^2)
             fOb <- sqrt(uData$obsvalue^2 + vData$obsvalue^2)
             fOb-fFgAn
           },
           "fg_dep-an_dep"={
             uFg <- uData$obsvalue-uData$fg_dep
             vFg <- vData$obsvalue-vData$fg_dep
             fFg <- sqrt(uFg^2 + vFg^2)
             fOb <- sqrt(uData$obsvalue^2 + vData$obsvalue^2)
             fFgDep <- fOb-fFg
             uAn <- uData$obsvalue-uData$an_dep
             vAn <- vData$obsvalue-vData$an_dep
             fAn <- sqrt(uAn^2 + vAn^2)
             fOb <- sqrt(uData$obsvalue^2 + vData$obsvalue^2)
             fAnDep <- fOb-fAn
             fFgDep-fAnDep
           },
           "obsvalue"={
             sqrt(uData$plotValues^2 + vData$plotValues^2)
           },
           {
             flog.error("Sorry, I don't know how to compute %s for ff.", dataColumn)
             NULL
           }
           )
  }
  buildColumn <- function(col) {
    switch(
        col,
        "latitude"=,
        "longitude"=,
        "statid"=,
        "active"=,
        "rejected"=,
        "passive"=,
        "blacklisted"=,
        "anflag"=,
        "level"=,
        "statid"=uData[col],
        "plotValues"=buildPlotValues(plotter$dataColumn),
        NULL
    )
  }
  colNames <- colnames(uData)
  ffData <- buildColumn(colNames[1])
  for (colName in colNames[2:length(colNames)]) {
    col <- buildColumn(colName)
    if (is.null(col)) {
      return(NULL)
    }
    ffData[colName] <- col
  }
  ffData
}
