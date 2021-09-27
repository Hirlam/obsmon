####################################################################
# Routines to help validate multiPlot entries from the config file #
# and make it easier to produce the multiPlots in server.R         #
####################################################################

multiPlotsGenId <- function(iPlot, type=NULL) {
  # Generate output IDs for the dinamically generated outpus
  qpName <- sprintf("multiPlot_%d", iPlot)
  if(is.null(type)) return(qpName)
  recogOutTypes <- c("plot", "map", "mapTitle", "queryUsed", "plotDataTable", "rawDataTable")
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
getMultiPlotConfig <- function(name, config=obsmonConfig) {
  # Helper function to retrieve the appropriate piece of the config file
  # given the name of a multiPlot
  rtn <- NULL
  for(pConf in config$multiPlots) {
    if(!(pConf$displayName==name)) next
    rtn <- pConf
    break
  }
  return(rtn)
}

multiPlotsMakeShinyInputs <- function(pConfig) {
  # Create a "shiny input"-like list that will be passed to the
  # ordinary plotting routines

  inputsForAllPlots <- list()
  plotsCommonInput <- list(
    experiment=pConfig$experiment,
    plottype=pConfig$plotType$name,
    odbBase=pConfig$database
  )

  # Prepare date/cycle inputs depending on the plot date type
  if(pConfig$plotType$dateType == "single") {
    # If the multiple dates/cycles are configured in this case, then
    # produce one single plot for each (date, cycle) combination.
    datesForSingleDtgPlot = paste0(seq(
      as.Date(pConfig$startDate),
      as.Date(pConfig$endDate),
      "days"
    ))
    cyclesForSingleDtgPlot = pConfig$cycles
  } else {
    # Setting these to NA because we'll loop over them later.
    # If setting to NULL, then the loop doesn't work (length==0).
    datesForSingleDtgPlot = NA
    cyclesForSingleDtgPlot = NA
    # Now populate the dates/cycles in the appropriate places for the
    # plot's "range" date type.
    plotsCommonInput <- c(
      plotsCommonInput,
      list(
        dateRange=c(pConfig$startDate, pConfig$endDate),
        cycles=pConfig$cycles
      )
    )
  }

  obnames <- names(pConfig$obs)
  iPlot <- 0
  for(iObname in seq_along(pConfig$obs)) {
    obname <- obnames[iObname]
    if(obname=="satem") {
      for(satemConfig in pConfig$obs[[obname]]) {
        # If the date type of the plot is "range", then datesForSingleDtgPlot
        # and cyclesForSingleDtgPlot are NA, and their loops has just 1 iter.
        for(date in datesForSingleDtgPlot) {
          for(cycle in cyclesForSingleDtgPlot) {
            inputsThisPlotOnly <- list(
              obname="satem",
              sensor=satemConfig$sensor,
              satellite=satemConfig$satellite,
              channels=satemConfig$channels,
              excludeChannels=satemConfig$excludeChannels,
              date=date,
              cycle=cycle
            )
            iPlot <- iPlot + 1
            inputsForAllPlots[[iPlot]] <- c(plotsCommonInput,inputsThisPlotOnly)
          }
        }
      }
    } else {
      levelsConfig <- pConfig$levels[[obname]]
      excludeLevelsConfig <- pConfig$excludeLevels[[obname]]
      stationsConfig <- pConfig$stations[[obname]]
      if(!pConfig$plotType$requiresSingleStation) {
        # One plot for each variable, allowing multiple stations in a
        # single plot (if stations are applicable at all)
        for(variable in unlist(pConfig$obs[iObname])) {
          stations <- unique(
            c(stationsConfig[["allVars"]], stationsConfig[[variable]])
          )
          # If the date type of the plot is "range", then datesForSingleDtgPlot
          # and cyclesForSingleDtgPlot are NA, and their loops has just 1 iter.
          for(date in datesForSingleDtgPlot) {
            for(cycle in cyclesForSingleDtgPlot) {
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
                station=stations,
                date=date,
                cycle=cycle
              )
              iPlot <- iPlot + 1
              inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
            }
          }
        }
      } else {
        # One plot for each variable and station
        for(variable in unlist(pConfig$obs[iObname])) {
          stations <- unique(
            c(stationsConfig[["allVars"]], stationsConfig[[variable]])
          )
          for(station in stations) {
            for(date in datesForSingleDtgPlot) {
              for(cycle in cyclesForSingleDtgPlot) {
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
                  station=station,
                  date=date,
                  cycle=cycle
                )
                iPlot <- iPlot + 1
                inputsForAllPlots[[iPlot]]<-c(plotsCommonInput,inputsThisPlotOnly)
              }
            }
          }
        }
      }
    }
  }
  return(inputsForAllPlots)
}

prepareMultiPlots <- function(
  plotType, inputsForAllPlots, db, modelDomain, progressFile=NULL
) {
  allPlots <- list()
  for(iPlot in seq_along(inputsForAllPlots)) {
   # Using a file to get update on progress of multiPlots
   # Unfortunately there was no other way to do this from within a future
   # at the time this code was written
   if(!is.null(progressFile)) {
     write(c(iPlot, length(inputsForAllPlots)), progressFile, append=FALSE)
   }
   newPlot <- obsmonPlotClass$new(
     parentType=plotType,
     db=db,
     paramsAsInUiInput=inputsForAllPlots[[iPlot]],
     modelDomain=modelDomain
   )
   newPlot$fetchRawData()
   allPlots[[multiPlotsGenId(iPlot)]] <- newPlot
  }
  return(allPlots)
}

prepareMultiPlotsCapturingOutput <- function(...) {
  # Same comments as in preparePlotsCapturingOutput (see plots.R) apply here.
  output <- capture.output({
    plots <- prepareMultiPlots(...)
  }, type="message")
  output <- trimws(paste(output, collapse="\n"))
  if(output=="") output <- character(0)
  return(list(plots=plots, output=output))
}

#############################
# Validating setup of plots #
#############################
dateInputFormat <- "%Y-%m-%d"
dateIsValid <- function(date, format=dateInputFormat) {
  if(is.null(date)) return(FALSE)
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
    rtn <- todayDateUTC - abs(startDate)
  } else if(dateIsValid(startDate, format)) {
    rtn <- as.Date(startDate, format)
  }
  return(rtn)
}
validateEndDate <- function(endDate, startDate=NULL, nDays=NULL, format=dateInputFormat) {
  rtn <- NULL
  if(dateIsValid(startDate)) {
    if(dateIsValid(endDate)) {
      rtn <- as.Date(endDate, format)
    } else if(is.integer(nDays) && nDays>0) {
      rtn <- startDate + nDays - 1
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
      '\n  multiPlot "%s": experiment "%s" not recognised',
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
      '\n  multiPlot "%s": database "%s" not recognised',
      plotConfig$displayName, plotConfig$database
    )
  }
  return(valid)
}

multiPlotPlotTypeNameValid <- function(plotConfig) {
  pType <- toLowerTrimAndSingleSpaces(plotConfig$plotType)
  valid <- isTRUE(
    pType %in% toLowerTrimAndSingleSpaces(names(plotRegistry$plotTypes))
  )
  if(!valid) {
    flog.error(
      '\n  multiPlot "%s": plotType "%s" not recognised',
      plotConfig$displayName, plotConfig$plotType
    )
  }
  return(valid)
}

datesCompatibleWithPlotType <- function(pConfig) {
  compatible <- TRUE
  msg <- ''
  if(is.null(pConfig$startDate) || is.null(pConfig$endDate)) {
    msg <- paste(
      msg,
      "Missing required date info. Needs one of the following: (i) startDate",
      "and endDate, (ii) startDate and nDays, or (iii) a negative startDate. "
    )
    compatible <- FALSE
  }
  if((pConfig$plotType$dateType=="single") && is.null(pConfig$cycles)) {
    msg <- paste(msg, "Missing required cycles info.")
    compatible <- FALSE
  }
  if(!compatible) {
    flog.error('\n  multiPlot "%s": %s', pConfig$displayName, msg)
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
    if(multiPlotPlotTypeNameValid(pc)) {
      # Replace plotType: Populate it with an obsmonPlotType obj
      tmpPlotTypeName <- toLowerTrimAndSingleSpaces(pc$plotType)
      for(plotTypeObj in plotRegistry$plotTypes) {
        if(tmpPlotTypeName == toLowerTrimAndSingleSpaces(plotTypeObj$name)) {
          pc$plotType <- plotTypeObj$copy()
          pc$plotType$interactive <- isTRUE(
            obsmonConfig$general$multiPlotsEnableInteractivity
          )
          break
        }
      }

      # Making sure stations are present if they are needed
      if(pc$plotType$requiresSingleStation && length(pc$stations)==0) {
        flog.error(
          paste(
            '\n  multiPlot "%s": Missing choice of stations',
            '(required by plotType "%s")'),
          pc$displayName, pc$plotType$name
        )
        validConfig <- FALSE
        missingStations <- TRUE
      }

      # Process dates
      pc$startDate <- validateStartDate(pc$startDate)
      pc$endDate <- validateEndDate(pc$endDate, pc$startDate, pc$nDays)
      if(!datesCompatibleWithPlotType(pc)) validConfig <- FALSE
    } else {
      validConfig <- FALSE
      invalidPlotNames <- TRUE
    }

    if(!validConfig) {
      flog.warn(
        '\n  multiPlot "%s": Failed to initialise. It will be ignored.',
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
      allowStationChoice <- (
        obSupportsStationChoice(obname) &&
        pc$plotType$supportsStationSelection
      )
      if(allowStationChoice) {
        if(!is.null(stationsConfig) && !is.list(stationsConfig)) {
          # If users set, e.g., "aircraft = 10" for levels in the config file
          pc$stations[[obname]] <- list(allVars=stationsConfig)
        }
      } else if(!is.null(stationsConfig)) {
        pc$stations[[obname]] <- NULL
        msg<-paste0('\n multiPlot "',pc$displayName,'": Combination of plot="',
          pc$plotType$name,'" and obname="', obname,
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
    msg <- "multiPlots: Required experiments not loaded!"
    if(length(exptNamesInConfig)>0) {
      msg <- "multiPlots: Please choose your experiment from:"
      for(exptName in exptNamesInConfig) msg <- paste0(msg,"\n  > ",exptName)
    }
    flog.warn(msg)
  }
  if(exists("invalidPlotNames")) {
    msg <- "multiPlots: Please choose your plotType from:"
    for(plotTypeName in names(plotRegistry$plotTypes)) {
      msg <- paste0(msg,"\n  > ",plotTypeName)
    }
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
