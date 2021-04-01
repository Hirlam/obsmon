plotClass <- setRefClass("obsmonPlot",
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
    plottingFunction="ANY"
  ),
  methods=list(
    ############################
    initialize = function(...) {
      callSuper(...)

      # Validate mandatory args
      for (field in c("name", "category", "dataX", "dataY")) {
        if(length(.self$field(field))==0) stop(sprintf("Missing parameter '%s'", field))
      }
      

      varnameRegex <- "^[a-zA-Z_$][a-zA-Z_$0-9]*$"
      # Validate dataX
      if(!isTRUE(grepl(varnameRegex, .self$dataX))) {
        stop(paste("Invalid value for the 'dataX' field:", .self$dataX))
      }

      # Validate dataY entries
      .self$dataY <- unique(.self$dataY)
      for (name in .self$dataY) {
        if(!isTRUE(grepl(varnameRegex, name))) {
          stop(paste("Field 'dataY' contains invalid column names:", name))
        }
      }

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
      return(isTRUE("station" %in% .self$getRetrievedSqliteFields()))
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
    uiInput2SqliteQuery = function(input) {
      # Previously named "plotBuildQuery"
      sqliteParams <- .self$uiInput2SqliteParams(input)
      return(sprintf(.self$getQueryStub(), buildWhereClause(sqliteParams)))
    },
    ############################
    uiInput2SqliteParams = function(input) {
      # Previously called "plotsBuildCriteria"
      res <- list()
      obname <- input$obname
      res$obnumber <- getAttrFromMetadata('obnumber', obname=obname)
      if (isTRUE(obname=='satem')) {
        res$obname <- input$sensor
        res$satname <- input$satellite
        levels <- input$channels
        excludeLevels <- input$excludeChannels
      } else {
        if (isTRUE(obname=='scatt')) {
          res$satname <- input$scatt_satellite
        }
        res$obname <- obname
        res$varname <- input$variable
        levels <- input$levels
        excludeLevels <- input$excludeLevels
      }

      res$levels <- list()
      if(length(levels)>0 && levels!="") res$levels <- levels
      res$excludeLevels <- list()
      if(length(excludeLevels)>0 && excludeLevels!="") {
        res$excludeLevels <- excludeLevels
      }

      if(obSupportsStationChoice(obname)) {
        station <- input$station
        if(is.null(station) || "" %in% station) station <- character(0)
        res$station <- station
      }

      res$dtg <- tryCatch(
        switch(.self$dateType,
          "single"=date2dtg(input$date, input$cycle),
          "range"={
            dateRange <- sort(input$dateRange)
            list(dateRange[1], dateRange[2], input$cycles)
          }
        ),
        error=function(e) NULL
      )

      return(res)
    }
  )
)
