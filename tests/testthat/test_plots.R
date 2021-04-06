workingDir <- getwd()
untar("test_data/mock_experiment.tgz", exdir="test_data")
setwd("../..")
sink("/dev/null")
source("src/init.R")
sink()
setwd(workingDir)


############################
context("PlotType objects")
############################

test_that("obsmonPlotType can be instanciated", {
  newPlot <- plotType(
    name="Name",
    category="Category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  expect_s4_class(newPlot, "obsmonPlotType")
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
  expect_false(newPlot$requiresSingleStation)
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
      expect_true(newPlot$requiresSingleStation)
   } else {
      expect_false(newPlot$requiresSingleStation)
   }
  }
})

test_that("'statid' is included in fields if stationChoiceType passed", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname"),
    stationChoiceType="single"
  )
  expect_true("statid" %in% newPlot$getRetrievedSqliteFields())
})

test_that("'usage' table is queried if station selection is supported", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname"),
    extraDataFields=list("statid")
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

test_that("ggplotlyWrapper produces plotly from ggplot", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  ggplotPlot <- ggplot() + theme_void()
  plotlyPlot <- newPlot$ggplotlyWrapper(ggplotPlot)
  expect_s3_class(plotlyPlot, "plotly")
})

test_that("ggplotlyWrapper returns original obj if not ggplot, with warning", {
  newPlot <- plotType(
    name="name",
    category="category",
    dataX="some_colname",
    dataY=list("some_colname")
  )
  randomObj <- list()

  # capture.output is being used to check for the warning content because
  # futile.logger warnings are not caught by testthat::expect_warning
  output <- capture.output(
    returnedObj <- newPlot$ggplotlyWrapper(randomObj),
    type="message"
  )
  expect_true(grepl(
    sprintf('Failure making plot "%s" interactive',  newPlot$name),
    output,
    fixed=TRUE
  ))

  expect_identical(randomObj, returnedObj)
})


#########################
context("Plot objects")
#########################

mockPlotType <- plotType(
  name="First Guess and Analysis Departure",
  category="Statistical",
  dataX="level",
  dataY=list("fg_bias_total", "an_bias_total", "fg_rms_total", "an_rms_total"),
  requiredDataFields=list("obnumber", "obname")
)

mockNonInteractivePlotType <- mockPlotType$copy()
mockNonInteractivePlotType$name <- "First Guess and Analysis Departure (static)"
mockNonInteractivePlotType$interactive <- FALSE

mockPlotTypeWithDataPP <- mockPlotType$copy()
mockPlotTypeWithDataPP$dataPostProcessingFunction <- function(data) {
  data$an_bias_total <- 2.0 * data$an_bias_total
  return (data)
}

mockUiInput <- list(
  obname="aircraft",
  variable="t",
  levels=c(22500, 27500, 35000, 45000),
  date=c("2019-08-06"),
  cycle=c(15),
  dateRange=c("2021-08-06", "2021-08-06"),
  cycles=c(0, 3, 6, 9, 12, 15, 18, 21)
)

obsmonDb <- obsmonDatabaseClass(
  dbType="ecma",
  dir="./test_data/mock_experiment/ecma"
)

test_that("obsmonPlot can be instanciated", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_s4_class(newPlot, "obsmonPlot")
})

test_that("obsmonPlot 'fetchRawData' works", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_equal(sum(dim(newPlot$rawData)), 0)
  newPlot$fetchRawData()
  expect_gt(prod(dim(newPlot$rawData)), 0)
  expect_setequal(
    colnames(newPlot$rawData),
    newPlot$parentType$getRetrievedSqliteFields()
  )
})

test_that("Accessing data automatically fetches rawData", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_equal(sum(dim(newPlot$rawData)), 0)
  data <- newPlot$data
  expect_gt(prod(dim(newPlot$rawData)), 0)
})

test_that("data contains all fields needed for X and Y", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_setequal(
    colnames(newPlot$data),
    c(newPlot$parentType$dataX, newPlot$parentType$dataY)
  )
})

test_that("dataPostProcessingFunction works", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  newPlotWithDataPP <- obsmonPlot(
    parentType=mockPlotTypeWithDataPP,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  testData <- newPlotWithDataPP$parentType$dataPostProcessingFunction(
    data.frame(newPlot$data)
  )
  expect_equal(newPlotWithDataPP$data, testData)
})

test_that("defaultGenerate function produces plotly if plot interactive", {
  newPlot <- obsmonPlot(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  graphicsObj <- newPlot$defaultGenerate()
  expect_s3_class(graphicsObj, "plotly")
})

test_that("defaultGenerate function produces ggplot if plot non-interactive", {
  newPlot <- obsmonPlot(
    parentType=mockNonInteractivePlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  graphicsObj <- newPlot$defaultGenerate()
  expect_s3_class(graphicsObj, "ggplot")
})

test_that("defaultGenerate function adds createdByDefaultGenerate attr", {
  for(parentType in c(mockPlotType, mockNonInteractivePlotType)) {
    newPlot <- obsmonPlot(
      parentType=parentType,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    graphicsObj <- newPlot$defaultGenerate()
    expect_true(attr(graphicsObj, "createdByDefaultGenerate"))
  }
})

test_that("'generate' uses 'defaultGenerate' if parentType$plottingFunction missing", {
  for(parentType in c(mockPlotType, mockNonInteractivePlotType)) {
    newPlot <- obsmonPlot(
      parentType=parentType,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    expect_s4_class(newPlot$parentType$plottingFunction, "uninitializedField")
    graphicsObj <- newPlot$generate()
    expect_true(attr(graphicsObj, "createdByDefaultGenerate"))
  }
})


###########################################
context("obsmonPlotRegistry")
###########################################
test_that("obsmonPlotRegistry can be instanciated", {
    plotRegistry <- obsmonPlotRegistry()
    expect_s4_class(plotRegistry, "obsmonPlotRegistry")
    expect_equal(length(plotRegistry$plotTypes), 0)
})

test_that("obsmonPlotRegistry can register plots with plotType args", {
    plotRegistry <- obsmonPlotRegistry()
    prevNumRegPlotTypes <- 0
    for(plotType in c(mockPlotType, mockNonInteractivePlotType)) {
      plotRegistry$registerPlotType(plotType)

      nRegPlotTypes <- length(plotRegistry$plotTypes)
      expect_equal(nRegPlotTypes, prevNumRegPlotTypes + 1)
      expect_equal(
        plotType,
        plotRegistry$plotTypes[[nRegPlotTypes]]
      )
      prevNumRegPlotTypes <- nRegPlotTypes
    }
})

test_that("obsmonPlotRegistry can register plots with regular args", {
  plotRegistry <- obsmonPlotRegistry()
  plotRegistry$registerPlotType(name="foo", category="bar")
  expect_equal(length(plotRegistry$plotTypes), 1)
  expect_equal(
    plotRegistry$plotTypes[[1]],
    plotType(name="foo", category="bar")
  )
})

test_that("obsmonPlotRegistry reg via args and plotType are equivalent", {
    names <- c("foo", "bar")
    categories <- c("baz", "qux")
    plotRegistry1 <- obsmonPlotRegistry()
    plotRegistry2 <- obsmonPlotRegistry()
    for (i in seq_along(names)) {
      plotRegistry1$registerPlotType(plotType(
        name=names[i],
        category=categories[i]
      ))
      plotRegistry2$registerPlotType(
        name=names[i],
        category=categories[i]
      )
    }
    expect_equal(plotRegistry1$plotTypes, plotRegistry2$plotTypes)
})

test_that("obsmonPlotRegistry refuses to register plots with same name", {
    plotType1 <- mockPlotType$copy()
    plotType2 <- mockNonInteractivePlotType$copy()
    plotType2$name <- plotType1$name

    plotRegistry <- obsmonPlotRegistry()
    plotRegistry$registerPlotType(plotType1)
    expect_error(
      plotRegistry$registerPlotType(plotType2),
      regex=sprintf(
        'Cannot register plot "%s": Name is already registered.',
        plotType2$name
      ),
      fixed=TRUE
    )
})

test_that("getCategorisedPlotTypeNames puts plotTypes in correct categories", {
  nPlotTypes <- 7
  nCategories <- nPlotTypes %/% 2
  plotNames <- paste("Plot Type", seq(nPlotTypes))
  uniqueCategories <- paste(
    "Category",
    stringi::stri_rand_strings(nCategories, 5, '[A-Z]')
  )
  plotCategories <- rep(uniqueCategories, length.out=nPlotTypes)

  plotRegistry <- obsmonPlotRegistry()
  for (iPlot in seq_along(plotNames)) {
    plotRegistry$registerPlotType(
      name=plotNames[iPlot],
      category=plotCategories[iPlot]
    )
  }

  categorisedPlotTypeNames <- plotRegistry$getCategorisedPlotTypeNames()
  expect_equal(length(categorisedPlotTypeNames), length(uniqueCategories))

  newPlotRegistry <- obsmonPlotRegistry()
  for (categ in uniqueCategories) {
    pTypeNamesInCategory <- categorisedPlotTypeNames[[categ]]
    for (pTypeName in pTypeNamesInCategory) {
      pType <- plotRegistry$plotTypes[[pTypeName]]
      expect_equal(pType$category, categ)
      newPlotRegistry$registerPlotType(pType)
    }
  }
  expect_mapequal(newPlotRegistry$plotTypes, plotRegistry$plotTypes)
})

test_that("isCompatibleWithUiParams works", {
  plotRegistry <- obsmonPlotRegistry()
  pType <- plotType(
    name="foo",
    category="foo category",
    requiredDataFields=list("statid")
  )
  expect_true(pType$isCompatibleWithUiParams(list(station="foo")))
  expect_false(pType$isCompatibleWithUiParams(list(station=character(0))))
  expect_false(pType$isCompatibleWithUiParams(list(station=NULL)))
  expect_false(pType$isCompatibleWithUiParams(list(obname="bar")))
})

test_that("getCategorisedPlotTypeNames works with compatibility filters", {
  plotRegistry <- obsmonPlotRegistry()
  plotRegistry$registerPlotType(
    name="foo",
    category="foo category",
    requiredDataFields=list("statid")
  )
  plotRegistry$registerPlotType(
    name="bar",
    category="bar category",
    requiredDataFields=list("obname")
  )

  fooCompatible <- plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(station="some station")
  )
  barCompatible <- plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(obname="some obname")
  )
  fullyIncompatibleMissingParamValue <-  plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(station=character(0))
  )
  expect_equal(names(fooCompatible), "foo category")
  expect_equal(names(barCompatible), "bar category")
  expect_equal(fullyIncompatibleMissingParamValue, list())
})


####################################################################
context("Implemented obsmon plots")
####################################################################

test_that("plotTypes in actual obsmon plotRegistry can produce plots", {
  for (pType in plotRegistry$plotTypes) {
    newPlot <- obsmonPlot(
      parentType=pType,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    graphicsObj <- newPlot$generate()
    expect_s3_class(graphicsObj, "plotly")
  }
})
