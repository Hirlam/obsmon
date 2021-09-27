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
  expect_s4_class(plotTypeClass(), "obsmonPlotType")
})

test_that("name is set correctly", {
  plotName <- "my new plot"
  newPlot <- plotTypeClass(name=plotName)
  expect_equal(newPlot$name, plotName)
})

test_that("category is set correctly", {
  plotCategory <- "my category"
  newPlot <- plotTypeClass(category=plotCategory)
  expect_equal(newPlot$category, plotCategory)
})

test_that("default dateType is 'single'", {
  expect_equal(plotTypeClass()$dateType, "single")
})

test_that("dateType must be one of 'single, 'range'", {
  expect_error(
    newPlot <- plotTypeClass(dateType="some_invalid_dateType"),
    regex="Field 'dateType' must be one of: 'single', 'range'",
    fixed=TRUE
  )

  for (dateType in c("single", "range")) {
    newPlot <- plotTypeClass(dateType=dateType)
    expect_equal(newPlot$dateType, dateType)
  }
})

test_that("Data fields cannot contain invalid varnames", {
  invalidColname <- "a name with spaces"
  for(field in c(
    "dataFieldsInSqliteWhereClause",
    "dataFieldsInRetrievedPlotData",
    "extraDataFields"
  )) {
    dataFieldArg <- list(list(invalidColname))
    names(dataFieldArg) <- field
    expect_error(
      do.call(plotTypeClass, dataFieldArg),
      regex=sprintf(
        "^Field '%s' contains invalid column names: %s$", field, invalidColname
      ),
    )
  }
})

test_that("queryStub contains dataFieldsInRetrievedPlotData and extraDataFields cols", {
  newPlot <- plotTypeClass(
    dataFieldsInRetrievedPlotData=list("col_a", "col_b", "col_c", "col_d"),
    extraDataFields=list("col_e", "col_f")
  )
  expected_cols <- paste(c(
    newPlot$dataFieldsInRetrievedPlotData,
    newPlot$extraDataFields
    ),
    collapse=", "
  )
  expect_true(
    grepl(paste("SELECT DISTINCT", expected_cols), newPlot$getQueryStub(), fixed=TRUE)
  )
})

test_that("non-function plottingFunction raises error", {
  expect_error(
    newPlot <- plotTypeClass(plottingFunction="foo"),
    regex="Field 'plottingFunction' is not a function",
    fixed=TRUE
  )
})

test_that("plottingFunction can be set to function", {
  plotFunc <- function(plot) {plot}
  newPlot <- plotTypeClass(plottingFunction=plotFunc)
  expect_identical(plotFunc, newPlot$plottingFunction)
})

test_that("non-function plotTitleFunction raises error", {
  expect_error(
    newPlot <- plotTypeClass(plotTitleFunction="foo"),
    regex="Field 'plotTitleFunction' is not a function",
    fixed=TRUE
  )
})

test_that("plotTitleFunction can be set to function", {
  plotTitleFunc <- function(plot) {""}
  newPlot <- plotTypeClass(plotTitleFunction=plotTitleFunc)
  expect_identical(plotTitleFunc, newPlot$plotTitleFunction)
})

test_that("default stationChoiceType is character(0)", {
  newPlot <- plotTypeClass()
  expect_identical(newPlot$stationChoiceType, character(0))
  expect_false(newPlot$requiresSingleStation)
})

test_that("stationChoiceType, if passed, must be one of 'single, 'multiple'", {
  expect_error(
    newPlot <- plotTypeClass(stationChoiceType="foo"),
    regex="Field 'stationChoiceType', if passed, should be one of: 'single', 'multiple'",
    fixed=TRUE
  )

  for (stationChoiceType in c("single", "multiple")) {
    newPlot <- plotTypeClass(stationChoiceType=stationChoiceType)
    expect_equal(newPlot$stationChoiceType, stationChoiceType)
    if(stationChoiceType == "single") {
      expect_true(newPlot$requiresSingleStation)
   } else {
      expect_false(newPlot$requiresSingleStation)
   }
  }
})

test_that("'statid' is included in fields if stationChoiceType passed", {
  newPlot <- plotTypeClass(stationChoiceType="single")
  expect_true("statid" %in% newPlot$getRetrievedSqliteFields())
})

test_that("'usage' table is queried if station selection is supported", {
  newPlot <- plotTypeClass(extraDataFields=list("statid"))
  expect_true(grepl("FROM usage", newPlot$getQueryStub(), fixed=TRUE))
})

test_that("'obsmon' table is queried if station selection is not supported", {
  expect_true(grepl("FROM obsmon", plotTypeClass()$getQueryStub(), fixed=TRUE))
})

#test_that("All needed data db columns are in the sqlite query", {
#  newPlot <- plotTypeClass(
#    name="name",
#    category="category",
#    dataX="some_colname",
#    dataY=list("a", "b", "c", "d"),
#    dataFieldsInSqliteWhereClause=list("e", "f"),
#    extraDataFields=list("g", "h")
#  )
#  patt <- "SELECT (.,[[:space:]]+)*%s(,?[[:space:]]+)(.+[[:space:]]+)?FROM"
#  for(property in c("dataX", "dataY", "dataFieldsInSqliteWhereClause", "extraDataFields")){
#    for (colname in newPlot$field(property)) {
#      print(paste(colname, newPlot$getQueryStub()))
#      expect_true(grepl(sprintf(patt, colname), newPlot$getQueryStub()))
#    }
#  }
#})

test_that("ggplotlyWrapper produces plotly from ggplot", {
  ggplotPlot <- ggplot() + theme_void()
  plotlyPlot <- plotTypeClass()$ggplotlyWrapper(ggplotPlot)
  expect_s3_class(plotlyPlot, "plotly")
})

test_that("ggplotlyWrapper returns original obj if not ggplot, with warning", {
  newPlot <- plotTypeClass(name="Foo Plot")
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

mockPlotType <- plotTypeClass(
  name="First Guess Departure Map",
  category="Maps",
  dateType="single",
  interactive=TRUE,
  dataFieldsInRetrievedPlotData=list(
    "latitude", "longitude", "level", "statid", "obsvalue", "fg_dep"
  ),
  dataFieldsInSqliteWhereClause=list("obnumber", "obname")
)

mockNonInteractivePlotType <- mockPlotType$copy()
mockNonInteractivePlotType$name <- "First Guess Departure Map (static)"
mockNonInteractivePlotType$interactive <- FALSE

mockPlotTypeWithDataPP <- mockPlotType$copy()
mockPlotTypeWithDataPP$dataPostProcessingFunction <- function(data, ...) {
  data$obsvalue <- 2.0 * data$obsvalue
  return (data)
}

mockUiInput <- list(
  obname="aircraft",
  variable="t",
  levels=c(22500, 27500, 35000, 45000),
  date=c("2019-08-06"),
  cycle=c(15),
  dateRange=c("2019-08-06", "2019-08-06"),
  cycles=c(0, 3, 6, 9, 12, 15, 18, 21)
)

obsmonDb <- obsmonDatabaseClass(
  dbType="ecma",
  dir="./test_data/mock_experiment/ecma"
)

test_that("obsmonPlotClass can be instanciated", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_s4_class(newPlot, "obsmonPlot")
})

test_that("obsmonPlotClass can be copied", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  # Trigger fetching data manually because this will be used in the comparisons
  newPlot$fetchRawData()

  for (shallow in c(TRUE, FALSE)) {
    plotCopy <- newPlot$copy(shallow=shallow)
    # Trigger fetching data because this will be used in the comparisons
    plotCopy$fetchRawData()
    expect_equal(class(newPlot), class(plotCopy))
    classFieldNamesToClasses <- newPlot$getRefClass()$fields()
    for (field in names(classFieldNamesToClasses)) {
      fieldClass <- classFieldNamesToClasses[[field]]
      if ("activeBindingFunction" %in% fieldClass) next
      expect_equal(class(newPlot$field(field)), class(plotCopy$field(field)))
      if(!shallow && field == "db") next
      expect_equal(newPlot$field(field), plotCopy$field(field))
    }
  }
})

test_that("obsmonPlotClass 'fetchRawData' works", {
  newPlot <- obsmonPlotClass$new(
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
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_equal(sum(dim(newPlot$rawData)), 0)
  data <- newPlot$data
  expect_gt(prod(dim(newPlot$rawData)), 0)
})

test_that("data contains all fields in dataFieldsInRetrievedPlotData", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_setequal(
    colnames(newPlot$data),
    newPlot$parentType$dataFieldsInRetrievedPlotData
  )
})

test_that("Hash changes if rawData modified", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  newPlot$fetchRawData()
  initialRawData <- newPlot$rawData
  initialHash <- newPlot$hash

  newPlot$rawData[,1] <- 10.0 * newPlot$rawData[,1]
  hashAfterModifyingData <- newPlot$hash
  expect_false(hashAfterModifyingData == initialHash)

  newPlot$fetchRawData()
  expect_equal(initialHash, newPlot$hash)
})

test_that("rawData can be modified", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  newPlot$fetchRawData()

  originalData <- newPlot$rawData
  expect_true("level" %in% colnames(originalData))

  newData <- data.frame(originalData, check.names=FALSE)
  multFactor <- 2
  newData$level <- multFactor * originalData$level

  newPlot$rawData <- newData
  expect_true(all(newPlot$rawData$level == newData$level))
  expect_true(all(newPlot$rawData$level == multFactor * originalData$level))
})

test_that("Hash changes if rawData modified", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  newPlot$fetchRawData()
  originalData <- newPlot$rawData
  initialHash <- newPlot$hash

  newPlot$rawData <- rbind(originalData, originalData)
  hashAfterModifyingData <- newPlot$hash

  expect_false(hashAfterModifyingData == initialHash)

  newPlot$rawData <- originalData
  expect_equal(initialHash, newPlot$hash)
})

test_that("dataPostProcessingFunction works", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  newPlotWithDataPP <- obsmonPlotClass$new(
    parentType=mockPlotTypeWithDataPP,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  testData <- newPlotWithDataPP$parentType$dataPostProcessingFunction(
    newPlot$data
  )
  expect_false(identical(newPlot$data, testData))
  expect_equal(newPlotWithDataPP$data, testData)
})

test_that(".defaultGenerate function produces plotly if plot interactive", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  graphicsObj <- newPlot$.defaultGenerate()
  expect_s3_class(graphicsObj, "plotly")
})

test_that(".defaultGenerate function produces ggplot if plot non-interactive", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockNonInteractivePlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  graphicsObj <- newPlot$.defaultGenerate()
  expect_s3_class(graphicsObj, "ggplot")
})

test_that(".defaultGenerate function adds createdByDefaultGenerate attr", {
  for(parentType in c(mockPlotType, mockNonInteractivePlotType)) {
    newPlot <- obsmonPlotClass$new(
      parentType=parentType,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    graphicsObj <- newPlot$.defaultGenerate()
    expect_true(attr(graphicsObj, "createdByDefaultGenerate"))
  }
})

test_that("chart uses '.defaultGenerate' if parentType$plottingFunction missing", {
  for(parentType in c(mockPlotType, mockNonInteractivePlotType)) {
    newPlot <- obsmonPlotClass$new(
      parentType=parentType,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    expect_s4_class(newPlot$parentType$plottingFunction, "uninitializedField")
    expect_true(attr(newPlot$chart, "createdByDefaultGenerate"))
  }
})

test_that("leafletMap returns NULL if no leafletPlottingFunction & 'map' not in category", {
  nonMapPlotType <- mockPlotType$copy()
  nonMapPlotType$category <- "Foo"
  newPlot <- obsmonPlotClass$new(
    parentType=nonMapPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_null(newPlot$leafletMap)
})

test_that("leafletMap is produced if no leafletPlottingFunction but 'map' in category", {
  mapPlotType <- plotTypeClass(
    name="Observation Usage",
    category="Maps",
    dateType="single",
    dataFieldsInRetrievedPlotData=list(
      "latitude", "longitude", "level", "statid", "active", "rejected",
      "passive", "blacklisted", "anflag", "obsvalue"
    ),
    dataFieldsInSqliteWhereClause=list("obnumber", "obname")
  )
  newPlot <- obsmonPlotClass$new(
    parentType=mapPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )
  expect_true("leaflet" %in% class(newPlot$leafletMap))
})

test_that("'dataWithUnits', 'chart' and 'leafletMap' cache results", {
  newPlot <- obsmonPlotClass$new(
    parentType=mockPlotType,
    db=obsmonDb,
    paramsAsInUiInput=mockUiInput
  )

  referenceData <- newPlot$dataWithUnits
  expect_equal(tracemem(referenceData), tracemem(newPlot$dataWithUnits))
  untracemem(referenceData)

  referenceGraph <- newPlot$chart
  expect_equal(tracemem(referenceGraph), tracemem(newPlot$chart))

  referenceGraph <- newPlot$leafletMap
  expect_equal(tracemem(referenceGraph), tracemem(newPlot$leafletMap))
})

###########################################
context("obsmonPlotRegistryClass")
###########################################
test_that("obsmonPlotRegistryClass can be instanciated", {
    plotRegistry <- obsmonPlotRegistryClass()
    expect_s4_class(plotRegistry, "obsmonPlotRegistry")
    expect_equal(length(plotRegistry$plotTypes), 0)
})

test_that("obsmonPlotRegistryClass can register plots with plotTypeClass args", {
    plotRegistry <- obsmonPlotRegistryClass()
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

test_that("obsmonPlotRegistryClass can register plots with regular args", {
  plotRegistry <- obsmonPlotRegistryClass()
  plotRegistry$registerPlotType(name="foo", category="bar")
  expect_equal(length(plotRegistry$plotTypes), 1)
  expect_equal(
    plotRegistry$plotTypes[[1]],
    plotTypeClass(name="foo", category="bar")
  )
})

test_that("obsmonPlotRegistryClass reg via args and plotTypeClass are equivalent", {
    names <- c("foo", "bar")
    categories <- c("baz", "qux")
    plotRegistry1 <- obsmonPlotRegistryClass()
    plotRegistry2 <- obsmonPlotRegistryClass()
    for (i in seq_along(names)) {
      plotRegistry1$registerPlotType(plotTypeClass(
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

test_that("obsmonPlotRegistryClass refuses to register plots with same name", {
    plotType1 <- mockPlotType$copy()
    plotType2 <- mockNonInteractivePlotType$copy()
    plotType2$name <- plotType1$name

    plotRegistry <- obsmonPlotRegistryClass()
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

  plotRegistry <- obsmonPlotRegistryClass()
  for (iPlot in seq_along(plotNames)) {
    plotRegistry$registerPlotType(
      name=plotNames[iPlot],
      category=plotCategories[iPlot]
    )
  }

  categorisedPlotTypeNames <- plotRegistry$getCategorisedPlotTypeNames()
  expect_equal(length(categorisedPlotTypeNames), length(uniqueCategories))

  newPlotRegistry <- obsmonPlotRegistryClass()
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
  plotRegistry <- obsmonPlotRegistryClass()
  pType <- plotTypeClass(
    name="foo",
    category="foo category",
    # "statid" is a special field whose absence should not lead to FALSE
    dataFieldsInSqliteWhereClause=list(
      "statid", "obname", list(varname="bar", level=10)
    )
  )

  # With station
  expect_true(pType$isCompatibleWithUiParams(list(
    station="foo",
    variable="bar",
    obname="baz",
    levels=10
  )))

  # Without station
  expect_true(pType$isCompatibleWithUiParams(list(
    obname="foo",
    variable="bar",
    levels=10
  )))

  # Failed element value in valued list
  expect_false(pType$isCompatibleWithUiParams(list(
    obname="foo",
    variable="qux",
    levels=10
  )))

  expect_false(pType$isCompatibleWithUiParams(list(station="some_station")))
  expect_false(pType$isCompatibleWithUiParams(list(obname=NULL)))
  expect_false(pType$isCompatibleWithUiParams(list(obnumber="foo")))
})

test_that("getCategorisedPlotTypeNames works with compatibility filters", {
  plotRegistry <- obsmonPlotRegistryClass()
  plotRegistry$registerPlotType(
    name="foo",
    category="foo category",
    # "statid" is a special field. See isCompatibleWithUiParams test.
    dataFieldsInSqliteWhereClause=list("statid", "obname")
  )
  plotRegistry$registerPlotType(
    name="bar",
    category="bar category",
    dataFieldsInSqliteWhereClause=list("varname")
  )

  fooCompatible <- plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(obname="some_obname")
  )
  barCompatible <- plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(variable="some_varname")
  )
  fullyIncompatibleMissingParamValue <-  plotRegistry$getCategorisedPlotTypeNames(
    compatibleWithUiInputParams=list(obname=character(0), varname=character(0))
  )
  expect_equal(names(fooCompatible), "foo category")
  expect_equal(names(barCompatible), "bar category")
  expect_equal(fullyIncompatibleMissingParamValue, list())
})


####################################################################
context("Implemented obsmon plots")
####################################################################

test_that("plotTypes in actual obsmon plotRegistry can produce static plots", {
  for (pType in plotRegistry$plotTypes) {
    pTypeStatic <- pType$copy()
    pTypeStatic$interactive <- FALSE

    newPlot <- obsmonPlotClass$new(
      parentType=pTypeStatic,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    expect_s3_class(newPlot$chart, c("ggplot", "gtable"))
  }
})

test_that("plotTypes in actual obsmon plotRegistry can produce interactive plots", {
  for (pType in plotRegistry$plotTypes) {
    pTypeInteractive <- pType$copy()
    pTypeInteractive$interactive <- TRUE

    newPlot <- obsmonPlotClass$new(
      parentType=pTypeInteractive,
      db=obsmonDb,
      paramsAsInUiInput=mockUiInput
    )
    testResult <- tryCatch(
      expect_s3_class(newPlot$chart, "plotly"),
      error=function(e) {
        sprintf("Failure in plot '%s'", pType$name)
        stop(e)
      }
    )
  }
})
