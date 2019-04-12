####################################################################
# Routines to help validate multiPlot entries from the config file #
# and make it easier to produce the multiPlots in server.R         #
####################################################################

multiPlotsGenId <- function(iPlot, type=NULL) {
  # Generate output IDs for the dinamically generated outpus
  qpName <- sprintf("multiPlot_%d", iPlot)
  if(is.null(type)) return(qpName)
  recogOutTypes <- c("plot", "map", "mapTitle", "queryUsed", "dataTable")
  if(!(type %in%  recogOutTypes)) {
    stop(sprintf(
      "multiPlotsGenId: Choose type from: %s",
      paste(recogOutTypes, collapse=", ")
    ))
  }
  return(sprintf("%s_%s", qpName, type))
}

#################################################
# Helper functions to produce plots in server.R #
#################################################
multiPlotsMakeShinyInputs <- function(pConfig) {
  # Create a "shiny input"-like list that will be passed to the
  # ordinary plotting routines

  inputsForAllPlots <- list()
  plotsCommonInput <- list(
    experiment=pConfig$experiment,
    plottype=pConfig$plotType,
    database=pConfig$database,
    date=pConfig$date,
    cycle=pConfig$cycle,
    dateRange=c(pConfig$startDate, pConfig$endDate),
    cycles=pConfig$cycles
  )

  obnames <- names(pConfig$obs)
  iPlot <- 0
  for(iObname in seq_along(pConfig$obs)) {
    obname <- obnames[iObname]
    if(obname=="satem") {
      for(satemConfig in pConfig$obs[[obname]]) {
        inputsThisPlotOnly <- list(
          obname="satem",
          sensor=satemConfig$sensor,
          satellite=satemConfig$satellite,
          channels=satemConfig$channels,
          excludeChannels=satemConfig$excludeChannels
        )
        iPlot <- iPlot + 1
        inputsForAllPlots[[iPlot]] <- c(plotsCommonInput,inputsThisPlotOnly)
      }
    } else {
      levelsConfig <- pConfig$levels[[obname]]
      excludeLevelsConfig <- pConfig$excludeLevels[[obname]]
      stationsConfig <- pConfig$stations[[obname]]
      if(!plotRequiresSingleStation(pConfig$plotType)) {
        # One plot for each variable, allowing multiple stations in a
        # single plot (if stations are applicable at all)
        for(variable in unlist(pConfig$obs[iObname])) {
          stations <- unique(
            c(stationsConfig[["allVars"]], stationsConfig[[variable]])
          )
          inputsThisPlotOnly <- list(
            obname=obname,
            variable=variable,
            levels=sort(unique(
              c(levelsConfig[["allVars"]], levelsConfig[[variable]])
            )),
            excludeLevels=sort(unique(c(
              excludeLevelsConfig[["allVars"]],
              excludeLevelsConfig[[variable]])
            )),
            station=stations
          )
          iPlot <- iPlot + 1
          inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
        }
      } else {
        # One plot for each variable and station
        for(variable in unlist(pConfig$obs[iObname])) {
          stations <- unique(
            c(stationsConfig[["allVars"]], stationsConfig[[variable]])
          )
          for(station in stations) {
            inputsThisPlotOnly <- list(
              obname=obname,
              variable=variable,
              levels=sort(unique(
                c(levelsConfig[["allVars"]], levelsConfig[[variable]])
              )),
              excludeLevels=sort(unique(c(
                excludeLevelsConfig[["allVars"]],
                excludeLevelsConfig[[variable]])
              )),
              station=station
            )
            iPlot <- iPlot + 1
            inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
          }
        }
      }
    }
  }
  return(inputsForAllPlots)
}

prepareMultiPlots <- function(
  plotter, inputsForAllPlots, db, progressFile=NULL
) {
  allPlots <- list()
  for(iPlot in seq_along(inputsForAllPlots)) {
   # Using a file to get update on progress of multiPlots
   # Unfortunately there was no other way to do this from within a future
   # at the time this code was written
   if(!is.null(progressFile)) {
     write(c(iPlot, length(inputsForAllPlots)), progressFile, append=FALSE)
   }
   # qp stands for "multiPlot"
   qpInput <- inputsForAllPlots[[iPlot]]
   plotRequest <- list()
   plotRequest$expName <- req(qpInput$experiment)
   plotRequest$dbType <- qpInput$database
   plotRequest$criteria <- plotsBuildCriteria(qpInput)

   newPlot <- tryCatch({
       preparePlots(plotter, plotRequest, db)
     },
     error=function(e) {flog.error(e); NULL}
   )
   allPlots[[multiPlotsGenId(iPlot)]] <- newPlot
  }
  return(allPlots)
}


#############################
# Validating setup of plots #
#############################
exptNamesInConfig <- c()
for(config in obsmonConfig$experiments) {
  exptNamesInConfig <- c(exptNamesInConfig, config$displayName)
}

dateInputFormat <- "%Y-%m-%d"
dateIsValid <- function(date, format=dateInputFormat) {
  tryCatch(!anyNA(as.Date(date, format)),
    error = function(e) FALSE
  )
}
validateStartDate <- function(startDate, format=dateInputFormat) {
  rtn <- NULL
  if(is.integer(startDate)) {
    # Allow startDate to be an integer<0, in which case it is considered
    # to represent a date startDate days before today
    todayDateUTC <- as.Date(strftime(Sys.Date(), tz="GM"))
    if(startDate<0) rtn <- todayDateUTC + startDate
  } else if(dateIsValid(startDate, format)) {
    rtn <- as.Date(startDate, format)
  }
}
validateEndDate <- function(endDate, startDate=NULL, nDays=NULL, format=dateInputFormat) {
  rtn <- NULL
  if(dateIsValid(startDate)) {
    if(dateIsValid(endDate)) {
      rtn <- as.Date(endDate, format)
    } else if(is.integer(nDays) && nDays>0) {
      rtn <- startDate + nDays
    } else {
      todayDateUTC <- as.Date(strftime(Sys.Date(), tz="GM"))
      rtn <- todayDateUTC
    }
  }
  return(rtn)
}

multiPlotExptValid <- function(plotConfig) {
  expt <- toLowerTrimAndSingleSpaces(plotConfig$experiment)
  valid <- isTRUE(expt %in% toLowerTrimAndSingleSpaces(exptNamesInConfig))
  if(!valid) {
    flog.error(
      'multiPlot "%s": experiment "%s" not recognised',
      plotConfig$displayName, plotConfig$experiment
    )
  }
  return(valid)
}

multiPlotDbValid <- function(plotConfig) {
  db <- tolower(plotConfig$database)
  valid <- isTRUE(db %in% tolower(dbTypesRecognised))
  if(!valid) {
    flog.error(
      'multiPlot "%s": database "%s" not recognised',
      plotConfig$displayName, plotConfig$database
    )
  }
  return(valid)
}

multiPlotPlotTypeValid <- function(plotConfig) {
  pType <- toLowerTrimAndSingleSpaces(plotConfig$plotType)
  valid <- isTRUE(pType %in% toLowerTrimAndSingleSpaces(names(plotTypesFlat)))
  if(!valid) {
    flog.error(
      'multiPlot "%s": plotType "%s" not recognised',
      plotConfig$displayName, plotConfig$plotType
    )
  }
  return(valid)
}

datesCompatibleWithPlotType <- function(pConfig) {
  compatible <- TRUE
  plotter <- plotTypesFlat[[pConfig$plotType]]
  if(plotter$dateType=="range") {
    if(is.null(pConfig$startDate) || is.null(pConfig$endDate)) {
      msg <- paste(
        "Selected plotType requires startDate and endDate (or",
        "startDate and nDays, or a negative startDate) to be configured."
      )
      compatible <- FALSE
    }
  } else {
    if(is.null(pConfig$date) || is.null(pConfig$cycle)) {
      msg <- "Selected plotType requires date and cycle to be set."
      compatible <- FALSE
    }
  }
  if(!compatible) {
    flog.error('multiPlot "%s": %s', pConfig$displayName, msg)
  }
  return(compatible)
}

multiPlotsValidateConfig <- function(config) {
  if(is.null(config$multiPlots)) return(config)
  flog.debug("Config file contains user-defined multiPlots. Validating.")

  for(iConfig in seq_along(config$multiPlots)) {
    # pc stands for "plot config"
    pc <- config$multiPlots[[iConfig]]
    validConfig <- TRUE
    if(multiPlotExptValid(pc)) {
      # Correct plotType excess spaces and character case
      pcExptName <- toLowerTrimAndSingleSpaces(pc$experiment)
      for(exactExptName in exptNamesInConfig) {
        if(toLowerTrimAndSingleSpaces(exactExptName) == pcExptName) {
          pc$experiment <- exactExptName
        }
      }
    } else {
      validConfig <- FALSE
      invalidExpts <- TRUE
    }
    if(multiPlotDbValid(pc)) {
      pc$database <- tolower(pc$database)
    } else {
      validConfig <- FALSE
      invalidDbs <- TRUE
    }
    if(multiPlotPlotTypeValid(pc)) {
      # Correct plotType excess spaces and character case
      tmpPlotType <- toLowerTrimAndSingleSpaces(pc$plotType)
      for(exactPlotName in names(plotTypesFlat)) {
        if(tmpPlotType == toLowerTrimAndSingleSpaces(exactPlotName)) {
          pc$plotType <- exactPlotName
          break
        }
      }
      # Process dates
      pc$date <- validateStartDate(pc$date)
      pc$startDate <- validateStartDate(pc$startDate)
      pc$endDate <- validateEndDate(pc$endDate, pc$startDate, pc$nDays)
      if(!datesCompatibleWithPlotType(pc)) validConfig <- FALSE
    } else {
      validConfig <- FALSE
      invalidPlotNames <- TRUE
    }

    if(!validConfig) {
      flog.warn(
        'Failed to initialise multiPlot "%s". It will be ignored.',
        pc$displayName
      )
      config$multiPlots[[iConfig]] <- NA
      next
    }

    # Process chosen obnames
    if(is.null(names(pc$obs))) {
      # In this case, the user has either:
      #   (i) Passed obs using the format obs = [obname1, obname2, ...],
      #       and we'll include all variables for the passed obnames
      #   (ii) Not specified any obname at all, in which case we'll include
      #        all variables for all obnames according to what has been
      #        registered in the file src/observation_definitions.R
      obnames <- unlist(pc$obs)
      if(is.null(obnames)) obnames <- getAttrFromMetadata("obname")
      pc$obs <- list()
      pc$obs[obnames] <- "all"
    }

    # Get list of variables to be excluded for all obs (if requested by user)
    # pc$excludeObs contains lists of variables as function of the obnames
    # TOML returns these lists as named vectors. Converting this one to a
    # proper R list in order to get NULL for non-existing entries instead of
    # getting "subscript out of bounds" errors
    allObsToBeRemoved <- as.list(pc$excludeObs)
    if(length(allObsToBeRemoved)>0 && is.null(names(allObsToBeRemoved))) {
      # In this case, the user has passed excludeObs using the format
      # excludeObs = [obname1, obname2, ...] and all variables need to
      # be excluded
      obnames <- unlist(allObsToBeRemoved)
      allObsToBeRemoved <- list()
      allObsToBeRemoved[obnames] <- "all"
    }

    # Populating variables (for non-satem observations) and
    # sensor/satellite/channels (for satem observations)
    for(obname in names(pc$obs)) {
      obsToRemove <- allObsToBeRemoved[[obname]]
      if(obname=="satem") {
        if(!is.list(obsToRemove) && isTRUE(obsToRemove=="all")) {
          # User has requested that all satem obs be removed
          pc$obs[["satem"]] <- NULL
          next
        }
        # At this point we know that there are satem obs to be included
        if(is.character(pc$obs[["satem"]]) && ("all" %in% pc$obs[["satem"]])){
          # User has either not explicitely listed any particular observation
          # type, or listed obs in teh format 'obs = [..., "satem", ...]'.
          # We'll thus add all satem obs obsmon knows about (according to info
          # registered in the file src/observation_definitions.R)
          iSatPlot <- 0
          pc$obs$satem <- list()
          allSensors <- getSensorNamesFromMetadata()
          for(sensor in allSensors) {
            allSatellites <- getSatelliteNamesFromMetadata(sensor)
            for(satellite in allSatellites) {
              newSatPlotConf <- list(sensor=sensor, satellite=satellite)
              iSatPlot <- iSatPlot + 1
              pc$obs$satem[[iSatPlot]] <- newSatPlotConf
            }
          }
        }
        if(!is.null(obsToRemove)) {
          # User has passed a list of specific satem obs to be removed.
          # The specification of sat obs to be excluded is expected to
          # include both sensor and satellite names, and may optionally
          # also include channels
          for(iConf in seq_along(pc$obs$satem)) {
            satConf <- pc$obs$satem[[iConf]]
            for(obToRemove in obsToRemove) {
              if(
                isTRUE(obToRemove$sensor==satConf$sensor) &&
                isTRUE(obToRemove$satellite==satConf$satellite)
              ) {
                if(is.null(obToRemove$channels)) {
                  # No channel specified, so we'll remove the whole entry
                  # Cannot set to NULL otherwise the entry will be removed
                  # and the loop indexing will loose meaning
                  pc$obs$satem[[iConf]] <- NA
                } else {
                  pc$obs$satem[[iConf]]$excludeChannels <- obToRemove$channels
                }
              }
            }
          }
          # Removing NA items
          pc$obs$satem <- Filter(Negate(anyNA), pc$obs$satem)
        }
      } else {
        vars <- NULL
        if(!("all" %in% obsToRemove)) {
          # For non-sat obs, pc$obs contains lists of variables as function
          # of the obnames
          vars <- pc$obs[[obname]]
          # If user doesn't specify any vars, use all variables from what has
          # been registered in the file src/observation_definitions.R
          if("all" %in% vars)vars<-getAttrFromMetadata("variables",obname=obname)
          vars <- vars[!(vars %in% obsToRemove)]
          if(length(vars)==0) vars <- NULL
        }
        pc$obs[[obname]] <- vars
      }
    }

    # Parsing level and station choices for non-satallite obs.
    # These can either be configured individually for each varname or
    # globally for each obname
    for(obname in names(pc$obs)) {
      # Levels and stations are not applicable to satem obs
      if(obname=="satem") next

      # (i) Parsing level choices
      # (i.a) Levels to be included
      levelsConfig <- pc$levels[[obname]]
      if(!is.null(levelsConfig) && !is.list(levelsConfig)) {
        # If users set, e.g., "aircraft = 10" for levels in the config file
        pc$levels[[obname]] <- list(allVars=levelsConfig)
      }
      # (i.b) Levels to be excluded
      excludeLevelsConfig <- pc$excludeLevels[[obname]]
      if(!is.null(excludeLevelsConfig) && !is.list(excludeLevelsConfig)) {
        # If users set, e.g., "aircraft = 10" for excludeLevels in the config
        pc$excludeLevels[[obname]] <- list(allVars=excludeLevelsConfig)
      }
      # (ii) Parsing station choices
      stationsConfig <- pc$stations[[obname]]
      obtype <- getAttrFromMetadata("category", obname=obname)
      if(plotSupportsChoosingStations(pc$plotType, obtype)) {
        if(!is.null(stationsConfig) && !is.list(stationsConfig)) {
          # If users set, e.g., "aircraft = 10" for levels in the config file
          pc$stations[[obname]] <- list(allVars=stationsConfig)
        }
      } else if(!is.null(stationsConfig)) {
        pc$stations[[obname]] <- NULL
        msg <- paste0('multiPlot "',pc$displayName,'": Plot "',pc$plotType,
          '" does not support station choices. Ignoring stations.'
        )
        flog.warn(msg)
      }
    }

    # Finally, save parsed config entry
    config$multiPlots[[iConfig]] <- pc
  }
  config$multiPlots <- Filter(Negate(anyNA), config$multiPlots)


  if(exists("invalidExpts")) {
    msg <- "multiPlots: Please choose your experiment from:"
    for(exptName in exptNamesInConfig) msg <- paste0(msg, "\n  > ", exptName)
    flog.warn(msg)
  }
  if(exists("invalidPlotNames")) {
    msg <- "multiPlots: Please choose your plotType from:"
    for(plotType in names(plotTypesFlat)) msg <- paste0(msg,"\n  > ",plotType)
    flog.warn(msg)
  }
  if(exists("invalidDbs")) {
    msg <- "multiPlots: Please choose your database from:"
    msg <- paste(msg, paste(dbTypesRecognised, collapse=", "))
    flog.warn(msg)
  }

  flog.debug("Finished validation of user-defined multiPlots.")

  return(config)
}

obsmonConfig <<- multiPlotsValidateConfig(obsmonConfig)
