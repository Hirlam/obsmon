plotType <- setRefClass(Class="obsmonPlotType",
  fields=list(
    name="character",
    category="character",
    dateType="character",
    dataFieldsInRetrievedPlotData="list",
    dataFieldsInSqliteWhereClause="list",
    extraDataFields="list",
    stationChoiceType="character",
    interactive="logical",
    dataPostProcessingFunction="ANY", # A function of 1 arg: data (data.frame)
    plottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    leafletPlottingFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
    plotTitleFunction="ANY", # A function of 1 arg: plot (an obsmonPlot object)
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
      dataFieldsInWhereClause <- c()
      for (item in .self$dataFieldsInSqliteWhereClause) {
        if (is.list(item)) {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, names(item))
        } else {
          dataFieldsInWhereClause <- c(dataFieldsInWhereClause, item)
        }
      }

      dbCols <- c(
        .self$dataFieldsInRetrievedPlotData,
        dataFieldsInWhereClause,
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

obsmonPlot <- setRefClass(Class="obsmonPlot",
  fields=list(
    parentType="obsmonPlotType",
    db="obsmonDatabase",
    paramsAsInUiInput="list",
    rawData="data.frame",
    .cache="list",
    ##############################
    hash = function(...) {
      components <- list()
      for (fieldName in names(.self$getRefClass()$fields())) {
        fieldClass <- .self$getRefClass()$fields()[[fieldName]]
        if(fieldClass == "activeBindingFunction") next
        if(fieldName %in% c("hash", ".cache")) next
        components <- c(components, .self$field(fieldName))
      }
      return(digest::digest(components))
    },
    chart = function(...) {return (.self$.memoise(FUN=.self$.generate, ...))},
    leafletMap = function(...) {
      return (.self$.memoise(FUN=.self$.generateLeafletMap, ...))
    },
    data = function(...) {
      return (.self$.memoise(FUN=.self$.getDataFromRawData, ...))
    },
    sqliteQuery = function(...) {.self$.getSqliteQuery()},
    paramsAsInSqliteDbs = function(...) {
      .self$parentType$getSqliteParamsFromUiParams(.self$paramsAsInUiInput)
    },
    title = function(...) {.self$.getTitle()}
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
      flog.trace("Done fetching raw data for plot '%s'", .self$title)
      if(is.null(fetchedData)) {
        colnames <- .self$parentType$getRetrievedSqliteFields()
        fetchedData <- data.frame(matrix(ncol=length(colnames), nrow=0))
        colnames(fetchedData) <- colnames
      }
      .self$rawData <- fetchedData
    },

    exportData = function(file, format) {
      format <- tolower(format)
      if(format=="csv") {
        write.csv(.self$data, file, row.names=FALSE)
      } else if (format=="txt") {
        write.table(.self$data, file, sep="\t", row.names=FALSE)
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
      plot <- tryCatch({
        if(nrow(.self$data)==0) {
          rtn <- errorPlot("Could not produce plot: No data retrieved.")
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
      return(plot)
    },

    .generateLeafletMap = function() {
      if(class(.self$parentType$leafletPlottingFunction) != "uninitializedField") {
        return(.self$parentType$leafletPlottingFunction(.self))
      }
      return(.self$.defaultGenerateLeafletMap())
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
      if(!("maps" %in% tolower(parentType$category))) return(NULL)
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

      if(length(.self$parentType$dataFieldsInRetrievedPlotData)>0) {
        rtn <- rtn[unlist(.self$parentType$dataFieldsInRetrievedPlotData)]
      }

      if(class(.self$parentType$dataPostProcessingFunction) != "uninitializedField") {
        rtn <- .self$parentType$dataPostProcessingFunction(rtn)
      }

      return(rtn)
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

      sqliteParams <- .self$paramsAsInSqliteDbs

      rtn <- ""
      if(length(.self$paramsAsInUiInput$experiment) > 0) {
        rtn <- paste0(.self$paramsAsInUiInput$experiment, ": ")
      }

      rtn <- paste0(rtn, .self$parentType$name, "\n")

      if(length(.self$paramsAsInUiInput$odbBase)>0) {
        rtn <- paste0(rtn, "db=", .self$paramsAsInUiInput$odbBase)
      }

      dtgs <- formatDtg(sqliteParams$dtg)
      if(length(dtgs)>0) {
        rtn <- sprintf("%s, DTG=%s", rtn, dtgs)
      }

      for (param in names(sqliteParams)) {
        if (param %in% c("dtg", "obnumber")) next
        if (param %in% .self$parentType$dataFieldsInRetrievedPlotData) next
        if (length(sqliteParams[[param]]) == 0) next
        vals <- sqliteParams[[param]]
        if (length(vals) > 1) {
          if(length(vals)>5) {
            vals <- c(vals[1:2], "...", vals[(length(vals)-1):length(vals)])
          }
          vals <- paste0("[", paste(vals, collapse=", "), "]")
        }
        rtn <- sprintf("%s, %s=%s", rtn, param, vals)
      }
      return (rtn)
    },

    .memoise = function(FUN, ...) {
      functionName <- substitute(FUN)
      functionHash <- digest::digest(functionName)
      cachedValue <- .self$.cache[[.self$hash]][[functionHash]]
      if(!is.null(cachedValue)) return(cachedValue)

      value <- FUN(...)
      newCacheEntry <- list(value)
      names(newCacheEntry) <- functionHash
      .self$.cache[[.self$hash]] <- c(.self$.cache[[.self$hash]], newCacheEntry)

      return(value)
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

levelsLableForPlots <- function(obnumber, varname=character(0)) {
  strObnumber <- as.character(obnumber)
  obstype <- getAttrFromMetadata("category", obnumber=obnumber)
  quantity <- "Pressure"
  if(obstype=="surface" || (isTRUE(strObnumber=="13") && !isTRUE(varname=="rh"))) {
    quantity <- "Height"
  }
  label <- sprintf("%s [%s]", quantity, getUnits(tolower(quantity)))
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
  dataColumnName <- unname(attributes(plotData)$comment["dataColumn"])
  if(is.null(dataColumnName)) {
    dataColumnName <- colnames(plotData)[ncol(plotData)]
  }

  dataRange <- range(plotData[[dataColumnName]])
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
