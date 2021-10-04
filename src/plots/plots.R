# SPINNER_CHART just shows a spinner at the middle of an empty plot
EMPTY_CHART <- plotly_empty(type="scatter", mode="markers") %>%
  config(
    displayModeBar=FALSE,
    scrollZoom=FALSE,
    editable=FALSE,
    staticPlot=TRUE
  )
SPINNER_CHART <- EMPTY_CHART %>%
  layout(
    images=list(
      list(
        # Add the spinner gif
        source=SPINNER_IMAGE_PATH,
        x=0.44,
        y=0.4125,
        sizex=0.3,
        sizey = 0.15,
        xref="paper",
        yref="paper",
        xanchor="left",
        yanchor="bottom"
      )
    )
  )
lockBinding("EMPTY_CHART", globalenv())
lockBinding("SPINNER_CHART", globalenv())

plotTypeClass <- setRefClass(Class="obsmonPlotType",
  fields=list(
    name="character",
    category="character",
    dateType="character",
    dataFieldsInRetrievedPlotData="list",
    dataFieldsInSqliteWhereClause="list",
    extraDataFields="list",
    stationChoiceType="character",
    interactive="logical",
    # dataPostProcessingFunction: A function where the 1st arg is
    # data (data.frame) and the 2nd, if used, is obsmonPlotObj (obsmonPlot)
    dataPostProcessingFunction="ANY",
    plottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    leafletPlottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    plotTitleFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    ############################
    flattenedDataFieldsInSqliteWhereClause = function(...) {
      dataFieldsInWhereClause <- c()
      for (item in .self$dataFieldsInSqliteWhereClause) {
        if (is.list(item)) {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, names(item))
        } else {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, item)
        }
      }
      return(dataFieldsInWhereClause)
    },
    supportsStationSelection = function(...) {
      return(isTRUE("statid" %in% .self$getRetrievedSqliteFields()))
    },
    requiresSingleStation = function(...) {
      return(isTRUE(.self$stationChoiceType == "single"))
    },
    queriedDbTable = function(...) {
      return(ifelse(.self$supportsStationSelection, "usage", "obsmon"))
    }
  ),
  methods=list(
    ############################
    initialize = function(...) {
      callSuper(...)

      if(length(.self$interactive)==0) {
        # Using "!isFALSE" to catch the "logical(0)" case and
        # interpret it as "TRUE"
        .self$interactive <- !isFALSE(
          obsmonConfig$general$plotsEnableInteractivity
        )
      }

      # Validate dataFieldsInSqliteWhereClause and extraDataFields entries
      varnameRegex <- "^[a-zA-Z_$][a-zA-Z_$0-9]*$"
      for(field in c(
        "dataFieldsInRetrievedPlotData",
        "dataFieldsInSqliteWhereClause",
        "extraDataFields"
      )) {
        .self$field(field, unique(.self$field(field)))
        if(length(.self$field(field))>0) {
          fieldValues <- .self$field(field)
          sqlColnames <- c()
          for (val in fieldValues) {
            if(is.list(val)) sqlColnames <- c(sqlColnames, names(val))
            else sqlColnames <- c(sqlColnames, val)
          }
          for (name in sqlColnames) {
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
        .self$dataFieldsInSqliteWhereClause <- c(
          .self$dataFieldsInSqliteWhereClause,
          "statid"
        )
      }

      # Validate fields ending with "Function"
      for (field in names(.self$getRefClass()$fields())) {
        if(!endsWith(field, "Function")) next
        func <- .self$field(field)
        if (class(func) != "uninitializedField" && typeof(func) != "closure") {
          stop(sprintf("Field '%s' is not a function", field))
        }
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
        .self$dataFieldsInRetrievedPlotData,
        .self$flattenedDataFieldsInSqliteWhereClause,
        .self$extraDataFields
      )
      return(unique(dbCols))
    },
    ############################
    getQueryStub = function() {
      # stationIDs are not stored in the "obsmon" table, only in "usage"
      whereStub <- "WHERE %s"
      if (
        (.self$queriedDbTable == "obsmon") &&
        !("nobs_total" %in% .self$getRetrievedSqliteFields())
      ) {
        whereStub <- paste(whereStub, "AND (nobs_total > 0)")
      }
      stub <- paste(
        "SELECT DISTINCT",
        paste(.self$getRetrievedSqliteFields(), collapse=", "),
        "FROM", .self$queriedDbTable, whereStub
      )
      return (stub)
    },
    ############################
    ggplotlyWrapper = function(ggplotPlot) {
      # Convert ggplot2 object ggplotPlot into a plotly object using
      # plotly's ggplotly function, and then apply some customisations
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
    },
    ############################
    isCompatibleWithUiParams = function(paramsAsInUiInput) {
      # See former "plotIsApplicable"
      sqliteParams <- .self$getSqliteParamsFromUiParams(paramsAsInUiInput)
      for (item in .self$dataFieldsInSqliteWhereClause) {
        if(is.list(item)) {
          paramsAndValues <- item
        } else {
          paramsAndValues <- list(NA)
          names(paramsAndValues) <- item
        }

        for (param in names(paramsAndValues)) {
          # statid is a bit special because, in the server, we use the
          # obsmonPlotType objects themselves to decide whether to show
          # or hide station selection menus
          if (param=="statid") next
          if (length(sqliteParams[[param]])==0) return (FALSE)
          paramValue <- paramsAndValues[[param]]
          if(!is.na(paramValue) && !isTRUE(sqliteParams[[param]]==paramValue)) {
            return (FALSE)
          }
        }
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
      # TODO: Make this more general (in case of future addition of new params)
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

.refClassObjHash <- function(.self) {
  componentsHashes <- c()
  for (fieldName in names(.self$getRefClass()$fields())) {
    fieldClass <- .self$getRefClass()$fields()[[fieldName]]

    # Skip fields that are actually methods under the hood
    if(fieldClass == "activeBindingFunction") next

    # Skip fields that are allowed to change
    if(fieldName %in% c("hash", ".cache")) next

    component <- .self$field(fieldName)
    # Hash just the function code, not environment or other attrs
    if(is.function(component)) component <- deparse(component)

    if(is(component, "envRefClass")) {
      # Parse recursively so that we also handle the special cases listed above
      componentsHash <- .refClassObjHash(component)
    } else {
      componentsHash <- digest::digest(component)
    }
    componentsHashes <- c(componentsHashes, componentsHash)
  }
  return(digest::digest(componentsHashes))
}

obsmonPlotClass <- setRefClass(Class="obsmonPlot",
  fields=list(
    parentType="obsmonPlotType",
    db="obsmonDatabase",
    paramsAsInUiInput="list",
    rawData="data.frame",
    modelDomain="domain",
    ##############################
    chart = function(newValue) {
      if(missing(newValue)) {
        return (.self$.memoise(FUN=.self$.generate))
      } else {
        flog.debug("Cannot set chart. Ignoring assignment.")
      }
    },
    leafletMap = function(newValue) {
      if(missing(newValue)) {
        return (.self$.memoise(FUN=.self$.generateLeafletMap))
      } else {
        flog.debug("Cannot set leafletMap. Ignoring assignment.")
      }
    },
    # ggplot2 doesn't like units: Remove them from data used in plots
    data = function(newValue) {
      if(missing(newValue)) {
        return(drop_units(.self$dataWithUnits))
      } else {
        flog.debug("Cannot set data. Ignoring assignment.")
      }
    },
    dataWithUnits = function(newValue) {
      if(missing(newValue)) {
        if(length(.self$rawData)==0) .self$fetchRawData()
        return(.self$.memoise(FUN=.self$.getDataFromRawData))
      } else {
        flog.debug("Cannot set dataWithUnits. Ignoring assignment.")
      }
    },
    sqliteQuery = function(...) {.self$.getSqliteQuery()},
    paramsAsInSqliteDbs = function(...) {
      .self$parentType$getSqliteParamsFromUiParams(.self$paramsAsInUiInput)
    },
    title = function(...) {.self$.getTitle()},
    hash = function(...) .refClassObjHash(.self),
    ##############################
    .cache="list"
  ),
  methods=list(
    fetchRawData = function(...) {
      flog.trace("Fetching raw data for plot '%s'...", .self$title)
      fetchedData <- performQuery(
        db=.self$db,
        query=.self$sqliteQuery,
        dtgs=.self$paramsAsInSqliteDbs$dtg,
        ...
      )
      if(is.null(fetchedData)) {
        colnames <- .self$parentType$getRetrievedSqliteFields()
        fetchedData <- data.frame(matrix(ncol=length(colnames), nrow=0))
        colnames(fetchedData) <- colnames
      }
      fetchedData <- fetchedData[do.call(order, fetchedData),]
      row.names(fetchedData) <- NULL
      .self$rawData <- fetchedData
      flog.trace("Done fetching raw data for plot '%s'", .self$title)
    },

    exportData = function(file, format, raw=FALSE) {
      if(raw) {
        dataToBeExported <- .self$rawData
      } else {
        dataToBeExported <- .self$data
      }

      format <- tolower(format)
      if(format=="csv") {
        write.csv(dataToBeExported, file, row.names=FALSE)
      } else if (format=="txt") {
        write.table(dataToBeExported, file, sep="\t", row.names=FALSE)
      } else {
        flog.error("Format '%s' not supported.", format)
        return(NULL)
      }

      dataInfo <- paste0(
        paste("# Plot title:", .self$title, "\n"),
        sprintf(
          "# Data retrieved by Obsmon v%s on %s using the following query:\n",
          obsmonVersion, strftime(Sys.time(), format="%Y-%m-%d %H:%M:%S %Z")
        ),
        paste0("# ", .self$sqliteQuery, "\n"),
        paste("\n")
      )
      write(paste0("\n", dataInfo), file, append=TRUE)
    },

    ############################
    .generate = function() {

      if(
        .self$modelDomain$grid$hasPoints &&
        isTRUE(grepl("maps", tolower(.self$parentType$category))) &&
        !isTRUE(.self$parentType$interactive)
      ) {
        flog.warn("Ignoring domain info: only supported in interactive maps.")
      }

      plot <- tryCatch({
        if(nrow(.self$data)==0) {
          rtn <- errorPlot("Could not produce plot: No data.")
        }else if(class(.self$parentType$plottingFunction) == "uninitializedField") {
          rtn <- .self$.defaultGenerate()
        } else {
          rtn <- .self$parentType$plottingFunction(.self)
          if(.self$parentType$interactive && !("plotly" %in% class(rtn))) {
            rtn <- .self$parentType$ggplotlyWrapper(rtn)
          }
        }
        rtn
      },
        error=function(e) {
          flog.error(e)
          errorPlot("Could not produce plot: Please check the logs.")
        }
      )

      plot <- plot %>% addTitleToPlot(.self$title)
      if("plotly" %in% class(plot)) {
        plot <- plot %>% configPlotlyWrapper()
      }

      return(plot)
    },

    .generateLeafletMap = function() {
      rtn <- tryCatch({
        if(class(.self$parentType$leafletPlottingFunction) != "uninitializedField") {
          .self$parentType$leafletPlottingFunction(.self)
        } else {
          .self$.defaultGenerateLeafletMap()
        }
      },
        error=function(e) {
          flog.error("Problems creating leaflet plot: %s", e)
          return(NULL)
        }
      )
      return(rtn)
    },

    .defaultGenerate = function() {
      # melt data so we can plot multiple curves (sharing same x-axis), each
      # with a different color and symbol
      data_colnames <- colnames(.self$data)
      df <- melt(.self$data, id=data_colnames[1])
      graph <- ggplot(data=df) +
        aes_string(
          x=data_colnames[1],
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

    .defaultGenerateLeafletMap = function() {
      if(!isTRUE(grepl("maps", tolower(parentType$category)))) return(NULL)
      # Use "check.names=FALSE" so that names such as "fg_dep+biascrl"
      # are not modified
      localPlotData <- data.frame(.self$data, check.names=FALSE)

      dataColumn <- unname(attributes(.self$data)$comment["dataColumn"])
      if(is.null(dataColumn)) {
        dataColumn <- colnames(.self$data)[ncol(.self$data)]
      }

      if(is.numeric(localPlotData[[dataColumn]])) {
        cm <- .getSuitableColorScale(localPlotData[dataColumn])
        dataPallete <- colorNumeric(palette=cm$palette, domain=cm$domain)
      } else {
        dataPallete <- colorFactor("RdYlBu", domain = NULL)
      }

      rtnLeafletMap <- leaflet(data=localPlotData) %>%
        addTiles() %>%
        addProviderTiles(
          "Esri.WorldStreetMap",
          options=providerTileOptions(opacity=0.5)
        ) %>%
        addCircleMarkers(
          lng=~longitude,
          lat=~latitude,
          popup=.getLeafletPopupContents(.self$data),
          fillColor=as.formula(sprintf("~dataPallete(`%s`)", dataColumn)),
          fillOpacity=0.5,
          stroke=FALSE,
          weight=1,
          opacity=1,
          color="black",
          clusterOptions=markerClusterOptions(disableClusteringAtZoom=6)
        ) %>%
        addLegend(
          "topright",
          pal=dataPallete,
          values=as.formula(sprintf("~`%s`", dataColumn)),
          opacity=1
        )
      return(rtnLeafletMap)
    },

    .getDataFromRawData = function() {
      if(length(.self$rawData)==0) .self$fetchRawData()
      rtn <- data.frame(.self$rawData, check.names=FALSE)

      # Filter out unwanted cols from the rawData
      if(length(.self$parentType$dataFieldsInRetrievedPlotData)>0) {
        selectedCols <- intersect(
          unlist(.self$parentType$dataFieldsInRetrievedPlotData),
          colnames(rtn)
        )
        rtn <- rtn[selectedCols]
      }

      # Add units
      rtn <- .self$.memoise(
        FUN=fillObsmonDataFrameWithUnits,
        df=rtn,
        # varname & obname are used to get the default units
        varname=.self$paramsAsInUiInput$variable,
        obname=.self$paramsAsInUiInput$obname,
        # These two lines provide info to enable unit conversions
        varUnits=.self$paramsAsInUiInput$variableUnits,
        levelsUnits=.self$paramsAsInUiInput$levelsUnits
      )

      if(
        isTRUE(.self$parentType$queriedDbTable == "usage") &&
        isTRUE(.self$paramsAsInUiInput$groupLevelsIntoStandardSwitch) &&
        ("level" %in% colnames(rtn))
      ) {
        # Group levels into reference/standard levels
        refLevels <- NULL
        if(ud_are_convertible(units(rtn$level), "Pa")) {
          refLevels <- refPressures
        } else if(ud_are_convertible(units(rtn$level), "m")) {
          refLevels <- refHeights
        }

        if(!is.null(refLevels)) {
          units(refLevels) <- units(rtn$level)
          refLevels <- drop_units(refLevels)

          reportedLevels <- rtn$level
          reportedLevel2Level <- Vectorize(function(reportedLevel) {
            refLevelIndex <- which.min(abs(refLevels - reportedLevel))
            return(refLevels[refLevelIndex])
          })
          rtn$level <- reportedLevel2Level(drop_units(reportedLevels))
          units(rtn$level) <- units(reportedLevels)
        }
      }

      # Apply eventual user-defined data post-processing
      if(class(.self$parentType$dataPostProcessingFunction) != "uninitializedField") {
        rtn <- .self$parentType$dataPostProcessingFunction(data=rtn, obsmonPlotObj=.self)
      }

      return(rtn[complete.cases(rtn),])
    },

    .getSqliteQuery = function() {
      # Previously named "plotBuildQuery"
      return(
        sprintf(
          .self$parentType$getQueryStub(),
          buildWhereClause(.self$paramsAsInSqliteDbs)
        )
      )
    },

    .getTitle = function() {
      if(class(.self$parentType$plotTitleFunction) != "uninitializedField") {
        return(.self$parentType$plotTitleFunction(.self))
      }

      valuesList2FormattedStr <- function(vals) {
        if (length(vals) > 1) {
          if(length(vals)>5) {
            vals <- c(vals[1:2], "...", vals[(length(vals)-1):length(vals)])
          }
          vals <- paste0("[", paste(vals, collapse=", "), "]")
        }
        return(vals)
      }

      sqliteParams <- .self$paramsAsInSqliteDbs

      rtn <- ""
      if(length(.self$paramsAsInUiInput$experiment) > 0) {
        rtn <- paste0(.self$paramsAsInUiInput$experiment, ": ")
      }

      rtn <- paste0(rtn, .self$parentType$name, "\n")

      if(length(.self$paramsAsInUiInput$station)>0) {
        rtn <- paste0(
          rtn,
          "station=",
          valuesList2FormattedStr(.self$paramsAsInUiInput$station),
          "\n"
        )
      }

      if(length(.self$paramsAsInUiInput$odbBase)>0) {
        rtn <- paste0(rtn, "db=", .self$paramsAsInUiInput$odbBase)
      }

      dtgs <- formatDtg(sqliteParams$dtg)
      if(length(dtgs)>0) {
        rtn <- sprintf("%s, DTG=%s", rtn, dtgs)
      }

      for (param in names(sqliteParams)) {
        if (param %in% c("dtg", "obnumber", "statid")) next
        if (param %in%
            setdiff(
              .self$parentType$dataFieldsInRetrievedPlotData,
              .self$parentType$flattenedDataFieldsInSqliteWhereClause
            )
        ) next
        if (length(sqliteParams[[param]]) == 0) next
        formattedVals <- valuesList2FormattedStr(sqliteParams[[param]])
        rtn <- sprintf("%s, %s=%s", rtn, param, formattedVals)
      }
      return (rtn)
    },

    .memoise = function(FUN, ...) {
      usedHash <- digest::digest(list(.self$hash, deparse(FUN), list(...)))
      if(is.null(.self$.cache[[usedHash]])) .self$.cache[[usedHash]] <- FUN(...)
      return(.self$.cache[[usedHash]])
    }
  )
)

obsmonPlotRegistryClass <- setRefClass(Class="obsmonPlotRegistry",
  fields=list(
    plotTypes="list"
  ),
  methods=list(
    registerPlotType = function(...) {
      args <- list(...)
      if(length(args)==1) {
        plot <- args[[1]]
      } else {
        plot <- plotTypeClass$new(...)
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
plotRegistry <- obsmonPlotRegistryClass()

#######################################################
# Some utility functions used when defining the plots #
#######################################################
errorPlot <- function(msg) {
  ggplot() +
    annotate("text", x=0, y=0, size=8, label=msg) +
    theme(
      panel.background = element_rect(fill="grey90"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
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

addTitleToPlot <- function(myPlot, title) {
  if(length(title)==0) {
    flog.warn("addTitleToPlot: Empty title")
    return(myPlot)
  }
  newPlot <- tryCatch({
    if(is.ggplot(myPlot)) {
      myPlot + ggtitle(title) + theme(plot.title=element_text(hjust=0.5))
    } else if("plotly" %in% class(myPlot)) {
      yTitle <- attr(myPlot, "yTitle")
      if(is.null(yTitle)) yTitle <- 1.0
      # Use an annotation instead of an actual title, as otherwise plotly
      # will fail to put it in the correct position without overlaps (and
      # it also won't allow users to change the position of the text)
      myPlot %>% add_annotations(
          text=title,
          showarrow=FALSE,
          font=list(size=20),
          xref="paper", xanchor="center", x=0.5,
          # Push y a bit above 1 as, otherwise, the title may overlap with the
          # plot when using ggplotly on a ggplot object containing facet_wraps
          yref="paper", yanchor="bottom", y=yTitle
      ) %>% layout(title=FALSE)
    } else {
      grid.arrange(myPlot, top=textGrob(title, gp=gpar(fontsize=13)))
    }
    },
    error=function(e) {
      flog.error("addTitleToPlot: Problems setting plot title: %s", e)
      myPlot
    }
  )
  return(newPlot)
}

.getSuitableColorScale <- function(plotData) {
  # Use a divergent colormat whenever there's change of sign
  # in the data, but use a sequential colormap otherwise.
  # Red/blue colors will represent +/- values.
  cm <- list(name=NULL, palette=NULL, direction=NULL, domain=NULL)

  if(is.data.frame(plotData)) {
    dataColumnName <- unname(attributes(plotData)$comment["dataColumn"])
    if(is.null(dataColumnName)) {
      dataColumnName <- colnames(plotData)[ncol(plotData)]
    }
    dataRange <- range(plotData[[dataColumnName]], na.rm=TRUE)
  } else {
    dataRange <- range(plotData, na.rm=TRUE)
  }
  # Use integers as default upper/lower limits
  dataRange <- c(floor(dataRange[1]), ceiling(dataRange[2]))

  if (prod(dataRange) >= 0) {
    spread <- diff(dataRange)
    if (sign(sum(dataRange)) < 0) {
      cm$name <- "Blues"
      cm$direction <- -1
      mincol <- dataRange[1]
      snapToZero <- dataRange[2]^2 < spread
      maxcol <- ifelse(snapToZero, 0., dataRange[2])
    } else {
      cm$name <- "Reds"
      cm$direction <- 1
      maxcol <- dataRange[2]
      snapToZero <- dataRange[1]^2 < spread
      mincol <- ifelse(snapToZero, 0., dataRange[1])
    }
  } else {
    cm$name <- "RdBu"
    cm$direction <- -1
    maxcol <- max(abs(dataRange))
    mincol <- -maxcol
  }
  cm$palette <- brewer.pal(brewer.pal.info[cm$name,]$maxcolors, cm$name)
  if(cm$direction < 0) cm$palette <- rev(cm$palette)
  cm$domain <- c(mincol, maxcol)
  return(cm)
}

.getLeafletPopupContents <- function(plotData) {
  plotData$popupContents <- ""
  for (colname in colnames(plotData)) {
    plotData$popupContents <- paste(
      plotData$popupContents,
      sprintf("%s: %s<br>", colname, plotData[[colname]])
    )
  }
  return(plotData[["popupContents"]])
}

fillDataWithQualityControlStatus <- function(data, ...) {
  status <- rep("NA", nrow(data))
  status <- ifelse(data$anflag == 0, "Rejected", status)
  status <- ifelse(data$active  > 0, "Active", status)
  status <- ifelse(data$rejected > 0, "Rejected", status)
  status <- ifelse(data$passive > 0, "Passive", status)
  status <- ifelse(data$blacklisted > 0, "Blacklisted", status)
  status <- ifelse(data$anflag  > 0, "Active(2)", status)
  status <- ifelse(data$anflag == 4, "Rejected", status)
  status <- ifelse(data$anflag == 8, "Blacklisted", status)
  status <- ifelse(data$anflag > 100, "Rejected in TITAN", status)
  status <- ifelse(data$anflag == 100, "TITAN: blacklisted", status)
  status <- ifelse(data$anflag == 101, "TITAN: nometa", status)
  status <- ifelse(data$anflag == 102, "TITAN: plaus", status)
  status <- ifelse(data$anflag == 103, "TITAN: clim", status)
  status <- ifelse(data$anflag == 104, "TITAN: buddy", status)
  status <- ifelse(data$anflag == 105, "TITAN: SCT", status)
  status <- ifelse(data$anflag == 106, "TITAN: DEM", status)
  status <- ifelse(data$anflag == 107, "TITAN: isol", status)
  status <- ifelse(data$anflag == 108, "TITAN: fg", status)
  status <- ifelse(data$anflag == 109, "TITAN: STEVE", status)
  status <- ifelse(data$anflag == 110, "TITAN: fge", status)
  status <- ifelse(data$anflag == 111, "TITAN: ccrrt", status)
  status <- ifelse(data$anflag == 112, "TITAN: puddle", status)
  status <- ifelse(data$anflag == 113, "TITAN: buddy_eve", status)
  status <- ifelse(data$anflag == 114, "TITAN: fg2", status)
  status <- ifelse(data$anflag == 151, "Fraction", status)
  status <- ifelse(data$anflag == 199, "Domain", status)

  data$status <- status
  data <- data[rev(order(data$status)),]
  return(data)
}
