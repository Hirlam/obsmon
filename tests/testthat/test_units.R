workingDir <- getwd()
untar("test_data/mock_experiment.tgz", exdir="test_data")
setwd("../..")
sink("/dev/null")
source("src/init.R")
sink()
setwd(workingDir)

##################
context("Units")
##################
getVarnames <- function(dbpath) {
  con <- dbConnect(RSQLite::SQLite(), dbpath, flags=RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(con))

  df <- data.frame(varname=NA)
  for(table in c("usage", "obsmon")) {
    query <- sprintf("SELECT DISTINCT varname FROM %s", table)
    df <- rbind(df, dbGetQuery(con, query))
  }
  return(unique(na.omit(df$varname)))
}

getData <- function(dbpath, table, varname) {
  con <- dbConnect(RSQLite::SQLite(), dbpath, flags=RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(con))

  query <- sprintf("SELECT * FROM %s WHERE varname=='%s'", table, varname)
  df <- dbGetQuery(con, query)
  # Remove empty columns
  df <- Filter(function(x)!all(is.na(x)), df)
  return(df)
}

test_that(".quantity2DefaultUnits elements belong to 'symbolic_units'", {
  expect_true(all(sapply(.quantity2DefaultUnits, class) == "symbolic_units"))
})

test_that("getUnits returns NULL, with a warning, for missing quantity", {
  output <- capture.output(
    expect_null(getUnits("foo")),
    type="message"
  )

  expect_true(grepl(
    "Could not determine units for quantity 'foo'",
    output,
    fixed=TRUE
  ))
})

test_that("fillObsmonDataFrameWithUnits works", {
  dbpath <- "test_data/mock_experiment/ecma/2019080615/ecma.db"
  varnames <- getVarnames(dbpath)
  for(varname in varnames) {
    obsvalueUnits <- getUnits(varname)
    expect_false(is.null(obsvalueUnits))
    for(table in c("usage", "obsmon")) {
      dfAllData <- getData(dbpath, table, varname)
      for (obname in unique(dfAllData$obname)) {
        df <- fillObsmonDataFrameWithUnits(subset(dfAllData, obname=obname))
        for(colname in colnames(df)) {
          colUnits <- tryCatch(
            units(df[[colname]]),
            error=function(e) NULL
          )
          if(colname %in% .dataColsWithoutUnits) {
            expect_null(colUnits)
          }
          else if (colname == "level") {
            expect_equal(colUnits, getUnitsForLevels(obname=obname, varname=varname))
          } else if (colname %in% c("latitude", "longitude")) {
            expect_equal(colUnits, getUnits("latlon"))
          } else {
            expect_equal(colUnits, obsvalueUnits)
          }
        }
      }
    }
  }
})
