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

context("Comparison regressions")

comparisonTestPlots <- function(parentType=mockPlotType, rawData=NULL) {
  plots <- lapply(c("expA", "expB", "expC"), function(experiment) {
    params <- mockUiInput
    params$experiment <- experiment
    plot <- obsmonPlotClass$new(
      parentType=parentType, db=obsmonDb, paramsAsInUiInput=params
    )
    if(!is.null(rawData)) plot$rawData <- rawData
    plot
  })
  plots[[1]]$compPlots <- plots[-1]
  plots[[1]]
}

test_that("no-data checks fetch lazily and still recognise empty queries", {
  plot <- comparisonTestPlots()
  expect_equal(nrow(plot$rawData), 0)
  expect_false(.plotHasNoDataRows(plot))
  expect_gt(nrow(plot$rawData), 0)
  plot$rawData <- plot$rawData[0, ]
  expect_true(.plotHasNoDataRows(plot))
})

test_that("active ObsFit computes RMS rather than centred standard deviation", {
  data <- data.frame(
    DTG=as.POSIXct("2019-08-06 15:00:00", tz="UTC"),
    level=32500, varname="t", fg_dep=c(3, 4, NA, 100),
    an_dep=c(6, 8, NA, 100), active=c(1, 1, 1, 0),
    rejected=0, passive=c(0, 0, 0, 1), blacklisted=0, anflag=0
  )
  units(data$fg_dep) <- "K"
  units(data$an_dep) <- "K"
  for(hasAnalysis in c(TRUE, FALSE)) {
    input <- if(hasAnalysis) data else data[names(data) != "an_dep"]
    result <- .obsFitActiveObsDataPostProcessingFunction(input)
    expect_equal(drop_units(result$fg_rms_total), sqrt(12.5))
    expect_equal(drop_units(result$fg_bias_total), 3.5)
    expect_equal(as.character(units(result$fg_rms_total)), "K")
    expect_equal(result$nobs_total, 3)
    if(hasAnalysis) expect_equal(drop_units(result$an_rms_total), sqrt(50))
  }
})

test_that("plot settings update all experiments and invalidate processed data", {
  plot <- comparisonTestPlots()
  experiments <- c(list(plot), .getComparisonPlots(plot))
  originalLevels <- lapply(experiments, function(p) p$data$level)
  originalValues <- lapply(experiments, function(p) p$data$obsvalue)
  originalRaw <- lapply(experiments, function(p) p$rawData)
  params <- list(
    levelsUnits="hPa", variableUnits="degC",
    groupLevelsIntoStandardSwitch=FALSE, minNobsForGriddedAverages=5
  )
  domain <- domainClass()
  .updateComparisonPlotSettings(plot, params=params, modelDomain=domain)
  for(i in seq_along(experiments)) {
    p <- experiments[[i]]
    expect_equal(p$paramsAsInUiInput[names(params)], params)
    expect_identical(p$modelDomain, domain)
    expect_equal(p$data$level, originalLevels[[i]] / 100)
    expect_equal(p$data$obsvalue, originalValues[[i]] - 273.15)
    expect_equal(p$rawData, originalRaw[[i]])
  }
})

test_that("station profile comparisons retain one legend per experiment and metric", {
  for(name in c("Station Vertical Profile: Obsvalue",
                "Station Vertical Profile: Obs, FG & Analysis")) {
    plot <- comparisonTestPlots(
      plotRegistry$plotTypes[[name]],
      data.frame(level=rep(c(32500, 40000), each=2),
                 obsvalue=280:283, fg_dep=1:4, an_dep=2:5)
    )
    chart <- plotly_build(plot$chart)
    legend <- Filter(function(trace) isTRUE(trace$showlegend), chart$x$data)
    metrics <- if(grepl("Obsvalue$", name)) "obsvalue" else c("obsvalue", "first_guess", "analysis")
    expected <- as.vector(outer(c("expA", "expB", "expC"), metrics, paste, sep=": "))
    expect_setequal(vapply(legend, `[[`, character(1), "name"), expected)
    expect_length(legend, length(expected))
  }
})

test_that("land-sea comparisons label every experiment and legend", {
  raw <- data.frame(
    DTG=as.POSIXct(c("2019-08-06 12:00:00", "2019-08-06 15:00:00"), tz="UTC"),
    level=1, nobs_total=5, nobs_land=2, nobs_sea=3,
    fg_uncorr_sea=1:2, fg_dep_sea=2:3, an_dep_sea=3:4,
    fg_uncorr_land=4:5, fg_dep_land=5:6, an_dep_land=6:7
  )
  plot <- comparisonTestPlots(plotRegistry$plotTypes[["Land-sea departures"]], raw)
  chart <- plotly_build(plot$chart)
  titles <- vapply(chart$x$layout$annotations, function(a) a$text, character(1))
  expect_true(all(c("expA", "expB", "expC") %in% titles))
  legend <- Filter(function(trace) isTRUE(trace$showlegend), chart$x$data)
  labels <- vapply(legend, `[[`, character(1), "name")
  for(experiment in c("expA", "expB", "expC")) {
    expect_true(paste0(experiment, ": fg_uncorr_sea") %in% labels)
    expect_true(paste0(experiment, ": nobs_land") %in% labels)
  }
  expect_true(all(grepl("^exp[ABC]: ", labels)))
})

test_that("Hovmoller comparisons share a colour range and a single colourbar", {
  raw <- data.frame(
    DTG=rep(as.POSIXct(c("2019-08-06 12:00:00", "2019-08-06 15:00:00"), tz="UTC"), 2),
    level=rep(1:2, each=2), nobs_total=5, fg_bias_total=1:4
  )
  plot <- comparisonTestPlots(plotRegistry$plotTypes[["Hovmöller"]], raw)
  plot$compPlots[[1]]$rawData$fg_bias_total <- 10:13
  plot$compPlots[[2]]$rawData$fg_bias_total <- -4:-1
  chart <- plotly_build(plot$chart)
  heatmaps <- Filter(function(trace) identical(trace$type, "heatmap"), chart$x$data)
  expect_length(heatmaps, 3)
  for(trace in heatmaps) {
    expect_equal(trace$zmin, -4)
    expect_equal(trace$zmax, 13)
    expect_equal(trace$colorscale, heatmaps[[1]]$colorscale)
  }
  expect_equal(sum(vapply(heatmaps, function(trace) isTRUE(trace$showscale), logical(1))), 1)
})

test_that("comparison legends use explicit labels for timeseries and statistics", {
  raw <- data.frame(
    DTG=rep(as.POSIXct(c("2019-08-06 12:00:00", "2019-08-06 15:00:00"), tz="UTC"), 2),
    level=rep(1:2, each=2), varname="t", nobs_total=5,
    fg_bias_total=1:4, an_bias_total=2:5,
    fg_rms_total=3:6, an_rms_total=4:7, fg_uncorr_total=5:8,
    fg_dep_total=1:4, an_dep_total=2:5
  )
  cases <- list(
    "ObsFit"=c("fg_rms_total", "an_rms_total", "fg_bias_total", "an_bias_total"),
    "Number of Observations"=NULL,
    "Bias Correction"=c("fg_corr", "fg_uncorr"),
    "Average First Guess and Analysis Bias/RMS"=c(
      "fg_bias_total_mean", "an_bias_total_mean", "fg_rms_total_mean", "an_rms_total_mean")
  )
  for(name in names(cases)) {
    plot <- comparisonTestPlots(plotRegistry$plotTypes[[name]], raw)
    chart <- plotly_build(plot$chart)
    legend <- Filter(function(trace) isTRUE(trace$showlegend), chart$x$data)
    labels <- vapply(legend, `[[`, character(1), "name")
    expected <- if(is.null(cases[[name]])) c("expA", "expB", "expC") else
      as.vector(outer(c("expA", "expB", "expC"), cases[[name]], paste, sep=": "))
    expect_setequal(labels, expected)
    expect_length(labels, length(expected))
  }
})

test_that("statistical line errorbars align with points for channels", {
  raw <- data.frame(
    DTG=rep(as.POSIXct(c("2019-08-06 12:00:00", "2019-08-06 15:00:00"), tz="UTC"), 2),
    level=rep(1:2, each=2), fg_bias_total=1:4, an_bias_total=2:5,
    fg_rms_total=3:6, an_rms_total=4:7
  )
  plot <- comparisonTestPlots(
    plotRegistry$plotTypes[["Average First Guess and Analysis Bias/RMS"]], raw)
  for(p in c(list(plot), .getComparisonPlots(plot))) {
    p$paramsAsInUiInput$obname <- "satem"
    p$paramsAsInUiInput$sensor <- "amsua"
    p$paramsAsInUiInput$variable <- "rad"
  }
  gg <- firstGuessAndAnPlottingFunction(plot)
  built <- ggplot_build(gg)
  points <- built$data[[2]]
  errors <- built$data[[3]]
  points <- points[order(points$group, points$x), ]
  errors <- errors[order(errors$group, errors$x), ]
  expect_equal(errors$x, points$x)
  expect_equal(errors$y, points$y)
  chart <- plotly_build(plot$parentType$ggplotlyWrapper(gg))
  legend <- Filter(function(trace) isTRUE(trace$showlegend), chart$x$data)
  expect_length(legend, 12)
  expect_true(all(vapply(legend, function(trace) trace$name == trace$legendgroup, logical(1))))

  for(p in c(list(plot), .getComparisonPlots(plot))) {
    p$paramsAsInUiInput$obname <- "synop"
    p$paramsAsInUiInput$variable <- "t2m"
  }
  chart <- plotly_build(plot$chart)
  legend <- Filter(function(trace) isTRUE(trace$showlegend), chart$x$data)
  expect_setequal(vapply(legend, `[[`, character(1), "name"), c("expA", "expB", "expC"))
  expect_length(legend, 3)
})

test_that("comparison progress continues through all experiment queries", {
  progressFile <- tempfile()
  on.exit(unlink(progressFile))
  plot <- comparisonTestPlots()
  plots <- c(list(plot), .getComparisonPlots(plot))
  for(i in seq_along(plots)) {
    plots[[i]]$fetchRawData(progressFile=progressFile, progressOffset=i-1, progressTotal=3)
    expect_equal(readPlotProgressFile(progressFile), list(current=i, total=3))
    expect_gt(nrow(plots[[i]]$rawData), 0)
  }
})

test_that("comparison caches do not change the primary hash or rerender charts", {
  type <- mockPlotType$copy()
  renders <- 0
  type$plottingFunction <- function(plot) {
    renders <<- renders + 1
    invisible(.combinePlotDataByExperiment(plot))
    ggplot() + theme_void()
  }
  type$category <- "Statistical"
  plot <- comparisonTestPlots(type)
  for(p in c(list(plot), .getComparisonPlots(plot))) p$fetchRawData()
  for(levelsUnits in c("Pa", "hPa")) {
    .updateComparisonPlotSettings(plot, params=list(levelsUnits=levelsUnits))
    hash <- plot$hash
    before <- renders
    chart <- plot$chart
    expect_equal(plot$hash, hash)
    expect_identical(plot$chart, chart)
    expect_equal(renders, before + 1)
  }
  hash <- plot$hash
  plot$compPlots[[1]]$.cache$extra <- "cached value"
  expect_equal(plot$hash, hash)
  plot$compPlots[[1]]$paramsAsInUiInput$levelsUnits <- "kPa"
  expect_false(identical(plot$hash, hash))
})

test_that("gridded comparison fills use the same scale as the colourbar", {
  panels <- lapply(list(c(-2, 1), c(1, 5), c(-5, 1)), function(values) {
    list(x=list(data=list(
      list(marker=list(color=values, colorbar=list(title="Value"), showscale=TRUE)),
      list(fill="toself", fillcolor="red", customdata=list(value=values[[1]])),
      list(fill="toself", fillcolor="blue", customdata=list(value=values[[2]]))
    )))
  })
  shared <- .shareComparisonMapColorScale(panels, c("expA", "expB", "expC"))
  cm <- .getSuitableColorScale(c(-2, 1, 1, 5, -5, 1))
  palette <- colorNumeric(cm$palette, cm$domain)
  for(panel in shared) {
    for(trace in panel$x$data[-1]) {
      expect_equal(trace$fillcolor, palette(trace$customdata$value))
    }
    expect_equal(panel$x$data[[1]]$marker$cmin, cm$domain[[1]])
    expect_equal(panel$x$data[[1]]$marker$cmax, cm$domain[[2]])
  }
  expect_equal(shared[[1]]$x$data[[3]]$fillcolor, shared[[2]]$x$data[[2]]$fillcolor)
  expect_equal(sum(vapply(shared, function(p) p$x$data[[1]]$marker$showscale, logical(1))), 1)
})

test_that("zero standard deviations keep comparison errorbars centred on bars", {
  raw <- data.frame(level=1, fg_bias_total=1:4, an_bias_total=2:5,
                    fg_rms_total=3:6, an_rms_total=4:7)
  plot <- comparisonTestPlots(
    plotRegistry$plotTypes[["Average First Guess and Analysis Bias/RMS"]], raw)
  plot$compPlots[[1]]$rawData$fg_bias_total <- 2
  plot$compPlots[[2]]$rawData$an_bias_total <- 3
  for(p in c(list(plot), .getComparisonPlots(plot))) {
    p$paramsAsInUiInput$obname <- "synop"
    p$paramsAsInUiInput$variable <- "t2m"
  }
  built <- ggplot_build(firstGuessAndAnPlottingFunction(plot))
  bars <- built$data[[1]]
  errors <- built$data[[2]]
  bars <- bars[order(bars$group, bars$x), ]
  errors <- errors[order(errors$group, errors$x), ]
  expect_equal(as.numeric(errors$x), as.numeric(bars$x))
  expect_equal(errors$y, bars$y)
  zero <- is.na(errors$ymin)
  expect_equal(sum(zero), 2)
  expect_true(all(is.na(errors$ymax[zero])))
  expect_null(getSdBarData(data.frame(level=1, fg_mean=2, fg_sd=0)))
})

test_that("map comparison overall titles sit above experiment titles", {
  raw <- data.frame(latitude=c(60, 61), longitude=c(20, 21),
                    level=32500, statid=c("a", "b"), obsvalue=280:281, fg_dep=1:2)
  plot <- comparisonTestPlots(
    plotRegistry$plotTypes[["First Guess Departure Map"]], raw)
  chart <- plotly_build(plot$chart)
  annotations <- chart$x$layout$annotations
  for(experiment in c("expA", "expB", "expC")) {
    panel <- Filter(function(a) identical(a$text, experiment), annotations)
    overall <- Filter(function(a) startsWith(a$text, "expA vs expB, expC:"), annotations)
    expect_length(panel, 1)
    expect_length(overall, 1)
    expect_gt(overall[[1]]$y, panel[[1]]$y)
  }
  expect_equal(chart$x$layout$margin$t, 150)
})

test_that("query file lookup and progress include hours between selected cycles", {
  requested <- NULL
  db <- list(getDataFilePaths=function(selectedDtgs) {
    requested <<- selectedDtgs
    as.character(selectedDtgs)
  })
  dtgs <- list("2019-08-06", "2019-08-06", c(12, 15))
  expect_equal(.getQueryDataFilePaths(db, dtgs), as.character(2019080612:2019080615))
  expect_equal(requested, 2019080612:2019080615)
  expect_equal(.getQueryDataFilePaths(db, 2019080615), "2019080615")
})

test_that("comparison database choices preserve valid selections on rebuild", {
  definitions <- parse("../../src/server/server_main_tab.R")
  wanted <- c(".getSelectedCompExperimentSlots", ".compOdbBaseInputId",
              ".getCompDbChoices", "output$compOdbBaseChoices")
  definitions <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      paste(deparse(expr[[2]]), collapse="") %in% wanted
  }, as.list(definitions))
  expect_length(definitions, 4)
  shiny::testServer(function(input, output, session) {
    expt <- list(dbs=list(
      ecma=list(dbType="ecma", dir=tempdir()),
      ccma=list(dbType="ccma", dir=tempdir())
    ))
    expts <- list(expB=expt, expC=expt, expD=expt)
    for(expr in definitions) eval(expr)
  }, {
    session$setInputs(enableComparison=TRUE, compExperiment="expB",
      compExperiment2="expC", compOdbBase_1="ccma", compOdbBase_2="ccma")
    selected <- function() {
      html <- output$compOdbBaseChoices$html
      matches <- gregexpr('value="ccma" selected', html, fixed=TRUE)[[1]]
      sum(matches > 0)
    }
    expect_equal(selected(), 2)
    session$setInputs(compExperiment="expD")
    expect_equal(selected(), 2)
    session$setInputs(compOdbBase_1="invalid", compExperiment="expB")
    expect_equal(selected(), 1)
  })
})

test_that("the fetch worker returns missing experiments after processing data", {
  findWorker <- function(expr) {
    if(!is.call(expr) && !is.expression(expr)) return(NULL)
    if(is.call(expr) && identical(expr[[1]], as.name("futureCall"))) {
      fun <- expr[["FUN"]]
      if("enableComparison" %in% names(formals(eval(fun)))) return(fun)
    }
    for(child in as.list(expr)) {
      worker <- findWorker(child)
      if(!is.null(worker)) return(worker)
    }
    NULL
  }
  worker <- eval(findWorker(parse("../../src/server/server_main_tab_plots.R")))
  expect_true(is.function(worker))
  type <- mockPlotType$copy()
  processed <- character(0)
  type$dataPostProcessingFunction <- function(data, obsmonPlotObj) {
    experiment <- obsmonPlotObj$primaryExperimentName
    processed <<- c(processed, experiment)
    if(experiment == "expB") data[0, ] else data
  }
  params <- mockUiInput
  params$experiment <- "expA"
  comparisonDbs <- lapply(c("expB", "expC"), function(name) {
    list(experimentName=name, db=obsmonDb)
  })
  result <- worker(parentType=type, db=obsmonDb, paramsAsInUiInput=params,
    modelDomain=domainClass(), enableComparison=TRUE, compDbs=comparisonDbs,
    primaryExperimentName="expA", progressFile=NULL)
  expect_equal(result$missingExperiments, "expB")
  expect_setequal(processed, c("expA", "expB", "expC"))
  processed <- character(0)
  result <- worker(parentType=type, db=obsmonDb, paramsAsInUiInput=params,
    modelDomain=domainClass(), enableComparison=FALSE, compDbs=list(),
    primaryExperimentName="expA", progressFile=NULL)
  expect_equal(result$missingExperiments, character(0))
  expect_length(processed, 0)
})
