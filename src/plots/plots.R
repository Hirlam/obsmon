plotType <- setRefClass(Class="obsmonPlotType",
  fields=list(
    name="character",
    category="character",
    dateType="character",
    requiredDataFields="list",
    extraDataFields="list",
    stationChoiceType="character",
    dataX="character", # Name of the field that will be in the plots' x
    dataY="list", # Names of the fields that will be in the plots' y
    xUnits="character",
    yUnits="character",
    interactive="logical",
    dataPostProcessingFunction="ANY", # A function of 1 arg: data (data.frame)
    plottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    ############################
    supportsStationSelection = function(...) {
      return(isTRUE("statid" %in% .self$getRetrievedSqliteFields()))
    },
    requiresSingleStation = function(...) {
      return(isTRUE(.self$stationChoiceType == "single"))
    }
  ),
  methods=list(
    ############################
    initialize = function(...) {
      callSuper(...)

      if(length(.self$dataX)==0) .self$dataX <- "x"
      if(length(.self$dataY)==0) .self$dataY <- list("y")

      # Validate dataX and dataY entries
      varnameRegex <- "^[a-zA-Z_$][a-zA-Z_$0-9]*$"

      if(!isTRUE(grepl(varnameRegex, .self$dataX))) {
        stop(paste("Invalid value for the 'dataX' field:", .self$dataX))
      }

      .self$dataY <- unique(.self$dataY)
      for (name in .self$dataY) {
        if(!isTRUE(grepl(varnameRegex, name))) {
          stop(paste("Field 'dataY' contains invalid column names:", name))
        }
      }

      # Make plot interative by default
      if(length(.self$interactive)==0) .self$interactive <- TRUE

      # Validate requiredDataFields and extraDataFields entries
      for(field in c("requiredDataFields", "extraDataFields")) {
        .self$field(field, unique(.self$field(field)))
        if(length(.self$field(field))>0) {
          for (name in .self$field(field)) {
            if(!isTRUE(grepl(varnameRegex, name))) {
              stop(sprintf("Field '%s' contains invalid column names: %s", field, name))
            }
          }
        }
      }

      # Validate stationChoiceType
      stationType <- .self$stationChoiceType
      if(length(stationType)>0 && !(stationType %in% c("single", "multiple"))) {
        stop("Field 'stationChoiceType', if passed, should be one of: 'single', 'multiple'")
      }

      # Make sure "statid" is listed as one of the wanted fields if
      # stationChoiceType is passed
      if (
        (length(.self$stationChoiceType) > 0) &&
        !("statid" %in% .self$getRetrievedSqliteFields())
      ) {
        .self$requiredDataFields <- c(.self$requiredDataFields, "statid")
      }

      # Validate plottingFunction
      func <- .self$plottingFunction
      if (class(func) != "uninitializedField" && typeof(func) != "closure") {
        stop("Field 'plottingFunction' is not a function")
      }

      # Validate dateType
      if(length(.self$dateType)==0) .self$dateType <- "single"
      if(!isTRUE(.self$dateType %in% c("single", "range"))) {
        stop("Field 'dateType' must be one of: 'single', 'range'")
      }
    },
    ############################
    getRetrievedSqliteFields = function() {
      dbCols <- c(
        .self$dataX,
        .self$dataY,
        .self$requiredDataFields,
        .self$extraDataFields
      )
      return(unique(dbCols))
    },
    ############################
    getQueryStub = function() {
      # stationIDs are not stored in the "obsmon" table, only in "usage"
      dbTable <- ifelse(.self$supportsStationSelection, "usage", "obsmon")
      stub <- paste(
        "SELECT",
        paste(.self$getRetrievedSqliteFields(), collapse=", "),
        "FROM", dbTable, "WHERE %s"
      )
      return (stub)
    },
    ############################
    ggplotlyWrapper = function(ggplotPlot) {
      # Generate a regular ggplot2 plot and then use plotly's
      # ggplotly function to convert it to a plotly object
      plotlyPlot <- tryCatch({
        ggplotly(ggplotPlot, tooltip=c("x","y")) %>%
          layout(
            margin=list(t=100),
            legend=list(orientation="v", yanchor="center", y=0.5)
          ) %>%
          configPlotlyWrapper()
        },
        error=function(e){
          flog.warn(
            'ggplotlyWrapper: Failure making plot "%s" interactive: %s',
            .self$name, e
          )
          return(ggplotPlot)
        }
      )
      return(plotlyPlot)
    },
    ############################
    isCompatibleWithUiParams = function(paramsAsInUiInput) {
      # See former "plotIsApplicable"
      sqliteParams <- .self$getSqliteParamsFromUiParams(paramsAsInUiInput)
      for (param in .self$requiredDataFields) {
        if (length(sqliteParams[[param]])==0) return (FALSE)
      }
      return (TRUE)
    },
    ############################
    getSqliteParamsFromUiParams = function(paramsAsInUiInput) {
      # Previously called "plotsBuildCriteria"
      # plotRequest <- list()
      # plotRequest$expName <- req(input$experiment)
      # plotRequest$dbType <- db$dbType
      # plotRequest$criteria <- plotsBuildCriteria(input)
      # For windspeed:
      # plotRequest$criteria$varname <- uName
      # plotRequest$criteria$varname <- vName
      res <- list()
      obname <- paramsAsInUiInput$obname
      res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
      if (isTRUE(obname=='satem')) {
        res$obname <- paramsAsInUiInput$sensor
        res$satname <- paramsAsInUiInput$satellite
        levels <- paramsAsInUiInput$channels
        excludeLevels <- paramsAsInUiInput$excludeChannels
      } else {
        if (isTRUE(obname=='scatt')) {
          res$satname <- paramsAsInUiInput$scatt_satellite
        }
        res$obname <- obname
        res$varname <- paramsAsInUiInput$variable
        levels <- paramsAsInUiInput$levels
        excludeLevels <- paramsAsInUiInput$excludeLevels
      }

      res$level <- list()
      if(length(levels)>0 && levels!="") res$level <- levels
      res$excludeLevels <- list()
      if(length(excludeLevels)>0 && excludeLevels!="") {
        res$excludeLevels <- excludeLevels
      }

      if(obSupportsStationChoice(obname)) {
        stations <- paramsAsInUiInput$station
        if(is.null(stations) || "" %in% stations) stations <- character(0)
        res$statid <- stations
      }

      res$dtg <- tryCatch(
        switch(.self$dateType,
          "single"=date2dtg(paramsAsInUiInput$date, paramsAsInUiInput$cycle),
          "range"={
            dateRange <- sort(paramsAsInUiInput$dateRange)
            list(dateRange[1], dateRange[2], paramsAsInUiInput$cycles)
          }
        ),
        error=function(e) NULL
      )

      return(res)
    }
  )
)

obsmonPlot <- setRefClass(Class="obsmonPlot",
  fields=list(
    parentType="obsmonPlotType",
    db="obsmonDatabase",
    paramsAsInUiInput="list",
    rawData="data.frame",
    ##############################
    data = function(...) {.self$getDataFromRawData()},
    sqliteQuery = function(...) {.self$getSqliteQuery()},
    paramsAsInSqliteDbs = function(...) {
      .self$parentType$getSqliteParamsFromUiParams(.self$paramsAsInUiInput)
    }
  ),
  methods=list(
    generate = function() {
      if(class(.self$parentType$plottingFunction) == "uninitializedField") {
        return(.self$defaultGenerate())
      }

      plot <- .self$parentType$plottingFunction(.self)
      if(.self$parentType$interactive && !("plotly" %in% class(plot))) {
        plot <- .self$parentType$ggplotlyWrapper(plot)
      }
      return(plot)
    },
    ############################
    defaultGenerate = function() {
      # melt data so we can plot multiple curves (sharing same x-axis), each
      # with a different color and symbol
      df <- melt(.self$data, id=.self$parentType$dataX)
      graph <- ggplot(data=df) +
        aes_string(
          x=.self$parentType$dataX,
          y="value",
          group="variable",
          colour="variable",
          shape="variable",
          fill="variable"
        ) +
        geom_point(size=4) +
        geom_line()

      if(.self$parentType$interactive) {
        graph <- .self$parentType$ggplotlyWrapper(graph)
      }

      attributes(graph)$createdByDefaultGenerate <- TRUE
      return(graph)
    },
    ############################
    getDataFromRawData = function() {
      if(length(.self$rawData)==0) .self$fetchRawData()
      dataColsToPlot <- c(
        .self$parentType$dataX,
        unlist(.self$parentType$dataY)
      )
      rtn <- .self$rawData[dataColsToPlot]
      if(class(.self$parentType$dataPostProcessingFunction) != "uninitializedField") {
        rtn <- .self$parentType$dataPostProcessingFunction(rtn)
      }
      return(rtn)
    },
    ############################
    fetchRawData = function() {
      .self$rawData <- performQuery(
        db=.self$db,
        query=.self$sqliteQuery,
        dtgs=.self$paramsAsInSqliteDbs$dtg
      )
    },
    ############################
    getSqliteQuery = function() {
      # Previously named "plotBuildQuery"
      return(
        sprintf(
          .self$parentType$getQueryStub(),
          buildWhereClause(.self$paramsAsInSqliteDbs)
        )
      )
    }
  )
)

obsmonPlotRegistry <- setRefClass(Class="obsmonPlotRegistry",
  fields=list(
    plotTypes="list"
  ),
  methods=list(
    registerPlotType = function(...) {
      args <- list(...)
      if(length(args)==1) {
        plot <- args[[1]]
      } else {
        plot <- plotType(...)
      }
      if(plot$name %in% names(.self$plotTypes)) {
        stop(sprintf(
          'Cannot register plot "%s": Name is already registered.',
          plot$name
        ))
      }
      newEntry <- list(plot)
      names(newEntry) <- plot$name
      .self$plotTypes <- c(.self$plotTypes, newEntry)
    },
    #########################
    getCategorisedPlotTypeNames = function(compatibleWithUiInputParams=NULL) {
      rtn <- list()

      if(is.null(compatibleWithUiInputParams)) {
        pTypes <- .self$plotTypes
      } else {
        pTypes <- list()
        for (pType in .self$plotTypes) {
          if(!pType$isCompatibleWithUiParams(compatibleWithUiInputParams)) next
          pTypes <- c(pTypes, pType)
        }
      }

      for (pType in pTypes) {
        if (pType$category %in% names(rtn)) {
          rtn[[pType$category]] <- c(rtn[[pType$category]], list(pType$name))
        } else {
          rtn[[pType$category]] <- list(pType$name)
        }
      }
      return(rtn)
    }
  )
)

# Create the plot registry that will be used throughout
plotRegistry <- obsmonPlotRegistry()

#######################################################
# Some utility functions used when defining the plots #
#######################################################
levelsLableForPlots <- function(obnumber, varname=character(0)) {
  strObnumber <- as.character(obnumber)
  obstype <- getAttrFromMetadata("category", obnumber=obnumber)
  quantity <- "Pressure"
  if(obstype=="surface" || (isTRUE(strObnumber=="13") && !isTRUE(varname=="rh"))) {
    quantity <- "Height"
  }
  label <- sprintf("%s [%s]", quantity, units[[tolower(quantity)]])
  return(label)
}

coord_flip_wrapper <- function(..., default=FALSE) {
  # Adds the argument "default" to the original ggplot's coord_flip.
  # This gets rid of the annoying "Coordinate system already present. Adding
  # new coordinate system, which will replace the existing one" warning which
  # is otherwise issued when trying to modify the x and y limits in plots that
  # use cood_flip
  # Adapted from <https://github.com/tidyverse/ggplot2/issues/2799>
  cf <- coord_flip(...)
  cf$default <- default
  return(cf)
}

configPlotlyWrapper <- function(...) {
  # Wrapper to plotly's config function, with some useful defaults
  # For a list of all config options, please visit
  # <https://github.com/plotly/plotly.js/blob/master/src/plot_api/plot_config.js>
  # Se allso <https://plotly-r.com/control-modebar.html>
  argList <- list(...)
  argNames <- names(argList)
  if(!("displaylogo" %in% argNames)) argList$displaylogo <- FALSE
  if(!("cloud" %in% argNames)) argList$cloud <- FALSE
  if(!("scrollZoom" %in% argNames)) argList$scrollZoom <- TRUE

  # Defaults for what users are allowed to edit in the plots
  if(!("editable" %in% argNames)) argList$editable <- TRUE
  editsOpts <- list(
    titleText=FALSE,
    shapePosition=FALSE
  )
  if("edits" %in% argNames) {
    for(name in names(argList$edits)) {
      editsOpts[[name]] <- argList$edits[[name]]
    }
  }
  argList$edits <- editsOpts

  # Defaults for saving figures
  plotlySaveAsFigDimensions <- list(height=755, width=1200)
  toImageButtonOpts <- list(
    filename="obsmon_plot",
    format="png",
    height=plotlySaveAsFigDimensions$height,
    width=plotlySaveAsFigDimensions$width
  )
  if("toImageButtonOptions" %in% argNames) {
    for(name in names(argList$toImageButtonOptions)) {
      toImageButtonOpts[[name]] <- argList$toImageButtonOptions[[name]]
    }
  }
  argList$toImageButtonOptions <- toImageButtonOpts

  return(do.call(config, argList))
}
