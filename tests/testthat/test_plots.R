context("Plotting engine")

workingDir <- getwd()
setwd("../..")
capture.output(source("src/init.R"))
capture.output(source("src/plots/plots.R"))
setwd(workingDir)

test_that("plot can be instaciated", {
  newPlot <- plotType(
    name="Name",
    category="Category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_s4_class(plot, "obsmonPlotType")
})

test_that("cannot create nameless plot", {
  expect_error(
    newPlot <- plotType(
      category="Category",
      dataX="some_colname",
      dataY=list("some_colname")
    ),
    regex="Missing parameter 'name'",
    fixed=TRUE
  )
})

test_that("name is set correctly", {
  plotName <- "my new plot"
  newPlot <- plotType(
    name=plotName,
    category="Category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_equal(newPlot$name, plotName)
})

test_that("cannot create plot without category", {
  expect_error(
    newPlot <- plotType(
      name="Name",
      dataX="some_colname",
      dataY=list("some_colname")
    ),
    regex="Missing parameter 'category'",
    fixed=TRUE
  )
})

test_that("category is set correctly", {
  plotCategory <- "my category"
  newPlot <- plotType(
    name="Name",
    category=plotCategory,
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_equal(newPlot$category, plotCategory)
})

test_that("default dateType is 'single'", {
  newPlot <- plotType(
    name="Name",
    category="Category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_equal(newPlot$dateType, "single")
})

test_that("dateType must be one of 'single, 'range'", {
  expect_error(
    newPlot <- plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      dateType="some_invalid_dateType"
    ),
    regex="Field 'dateType' must be one of: 'single', 'range'",
    fixed=TRUE
  )

  for (dateType in c("single", "range")) {
    newPlot <- plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      dateType=dateType
    )
    expect_equal(newPlot$dateType, dateType)
  }
})

test_that("datax cannot be an invalid varname", {
  invalidColname <- "a name with spaces"
  expect_error(
    plotType(
      name="name",
      category="category",
      dataX=invalidColname,
      dataY=list("some_colname")
    ),
    regex=paste0(
      "^Invalid value for the 'dataX' field: ",
      invalidColname, "$"
    ),
  )
})

test_that("dataY cannot contain invalid varnames", {
  invalidColname <- "a name with spaces"
  expect_error(
    plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("colname_1", invalidColname)
    ),
    regex=paste0(
      "^Field 'dataY' contains invalid column names: ",
      invalidColname, "$"
    ),
  )
})

test_that("extraDataFields cannot contain invalid varnames", {
  invalidColname <- "a name with spaces"
  expect_error(
    plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      extraDataFields=list("colname_1", invalidColname)
    ),
    regex=paste0(
      "^Field 'extraDataFields' contains invalid column names: ",
      invalidColname, "$"
    ),
  )
})

test_that("queryStub contains cols from dataX, dataY and extraDataFields", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="col_a",
    dataY=list("col_b", "col_c", "col_d"),
    extraDataFields=list("col_e", "col_f")
  )
  expected_cols <- paste(c(
    newPlot$dataX,
    newPlot$dataY,
    newPlot$extraDataFields
    ),
    collapse=", "
  )
  expect_true(
    grepl(paste("SELECT", expected_cols), newPlot$getQueryStub(), fixed=TRUE)
  )
})

test_that("non-function plottingFunction raises error", {
  expect_error(
    newPlot <- plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      plottingFunction="A"
    ),
    regex="Field 'plottingFunction' is not a function",
    fixed=TRUE
  )
})

test_that("plottingFunction can be set to function", {
  plotFunc <- function(data) {data}
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname"),
    plottingFunction=plotFunc
  )
  expect_identical(plotFunc, newPlot$plottingFunction)
})

test_that("default stationChoiceType is character(0)", {
  newPlot <- plotType(
    name="Name",
    category="Category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_identical(newPlot$stationChoiceType, character(0))
  expect_false(newPlot$requiresSingleStation())
})

test_that("stationChoiceType, if passed, must be one of 'single, 'multiple'", {
  expect_error(
    newPlot <- plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      stationChoiceType="some_invalid_stationChoiceType"
    ),
    regex="Field 'stationChoiceType', if passed, should be one of: 'single', 'multiple'",
    fixed=TRUE
  )

  for (stationChoiceType in c("single", "multiple")) {
    newPlot <- plotType(
      name="name",
      category="category",
      dataX="some_colname",
      dataY=list("some_colname"),
      stationChoiceType=stationChoiceType
    )
    expect_equal(newPlot$stationChoiceType, stationChoiceType)
    if(stationChoiceType == "single") {
      expect_true(newPlot$requiresSingleStation())
   } else {
      expect_false(newPlot$requiresSingleStation())
   }
  }
})

test_that("'station' is included in fields if stationChoiceType passed", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname"),
    stationChoiceType="single"
  )
  expect_true("station" %in% newPlot$getRetrievedSqliteFields())
})

test_that("'usage' table is queried if station selection is supported", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname"),
    extraDataFields=list("station")
  )
  expect_true(
    grepl("FROM usage", newPlot$getQueryStub(), fixed=TRUE)
  )
})

test_that("'obsmon' table is queried if station selection is not supported", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_true(
    grepl("FROM obsmon", newPlot$getQueryStub(), fixed=TRUE)
  )
})

# Test that table is "usage" if station in needed sqlite cols
# Test that table is "obsmon" if station not in needed sqlite cols
