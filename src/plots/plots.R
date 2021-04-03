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
    plottingFunction="ANY" # A function of 1 arg: plot (an obsmonPlot object)
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
    supportsStationSelection = function() {
      return(isTRUE("statid" %in% .self$getRetrievedSqliteFields()))
    },
    ############################
    requiresSingleStation = function() {
      return(isTRUE(.self$stationChoiceType == "single"))
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
      dbTable <- ifelse(.self$supportsStationSelection(), "usage", "obsmon")
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
          )
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
    }
  )
)

obsmonPlot <- setRefClass(Class="obsmonPlot",
  fields=list(
    parentType="obsmonPlotType",
    db="obsmonDatabase",
    params="list", # List like the shiny "input" from the UI
    rawData="data.frame",
    data=function() {
      if(length(.self$rawData)==0) .self$fetchRawData()
      dataColsToPlot <- c(
        .self$parentType$dataX,
        unlist(.self$parentType$dataY)
      )
      return(.self$rawData[dataColsToPlot])
    },
    sqliteQuery = function() {
      # Previously named "plotBuildQuery"
      return(
        sprintf(
          .self$parentType$getQueryStub(),
          buildWhereClause(.self$getSqliteCriteriaFromParams())
        )
      )
    }
  ),
  methods=list(
    fetchRawData = function() {
      .self$rawData <- performQuery(
        db=.self$db,
        query=.self$sqliteQuery,
        dtgs=.self$getSqliteCriteriaFromParams()$dtg
      )
    },
    ############################
    defaultGenerate = function() {
      # melt data so we can plot multiple curves (sharing same x-axis), each
      # with a different color and symbol
      df <- melt(.self$data, id=.self$parentType$dataX)
      if(.self$parentType$interactive) {
        graph <- plot_ly(
          df,
          x=as.formula(paste0("~", .self$parentType$dataX)),
          y=~value,
          type="scatter",
          mode='lines+markers',
          marker = list(size=15),
          color=~variable,
          symbol=~variable
        )
      } else {
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
      }
      attributes(graph)$createdByDefaultGenerate <- TRUE
      return(graph)
    },
    ############################
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
    getSqliteCriteriaFromParams = function() {
      # Previously called "plotsBuildCriteria"
      # plotRequest <- list()
      # plotRequest$expName <- req(input$experiment)
      # plotRequest$dbType <- db$dbType
      # plotRequest$criteria <- plotsBuildCriteria(input)
      # For windspeed:
      # plotRequest$criteria$varname <- uName
      # plotRequest$criteria$varname <- vName
      res <- list()
      obname <- .self$params$obname
      res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
      if (isTRUE(obname=='satem')) {
        res$obname <- .self$params$sensor
        res$satname <- .self$params$satellite
        levels <- .self$params$channels
        excludeLevels <- .self$params$excludeChannels
      } else {
        if (isTRUE(obname=='scatt')) {
          res$satname <- .self$params$scatt_satellite
        }
        res$obname <- obname
        res$varname <- .self$params$variable
        levels <- .self$params$levels
        excludeLevels <- .self$params$excludeLevels
      }

      res$levels <- list()
      if(length(levels)>0 && levels!="") res$levels <- levels
      res$excludeLevels <- list()
      if(length(excludeLevels)>0 && excludeLevels!="") {
        res$excludeLevels <- excludeLevels
      }

      if(obSupportsStationChoice(obname)) {
        station <- .self$params$station
        if(is.null(station) || "" %in% station) station <- character(0)
        res$station <- station
      }

      res$dtg <- tryCatch(
        switch(.self$parentType$dateType,
          "single"=date2dtg(.self$params$date, .self$params$cycle),
          "range"={
            dateRange <- sort(.self$params$dateRange)
            list(dateRange[1], dateRange[2], .self$params$cycles)
          }
        ),
        error=function(e) NULL
      )

      return(res)
    }
  )
)
