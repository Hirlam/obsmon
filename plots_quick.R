####################################################################
# Routines to help validate quickPlot entries from the config file #
# and make it easier to produce the quickPlots in server.R         #
####################################################################

dateInputFormat <- "%Y-%m-%d"
dateIsValid <- function(date, format=dateInputFormat) {
  tryCatch(!is.na(as.Date(date, format)),
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

validateOneClickPlotConfig <- function(config) {
  invalidExpts <- c()
  invalidPlotNames <- c()
  invalidDbs <- c()

  if(is.null(config$quickPlots)) return(config)

  flog.debug("Config file contains user-defined quickPlots. Validating.")

  availablePlots <- names(plotTypesFlat)
  for(iConfig in seq_along(config$quickPlots)) {
    # pc stands for "plot config"
    pc <- config$quickPlots[[iConfig]]
    validExpt <- isTRUE(pc$experiment %in% exptNamesinConfig)
    validPlotType <- isTRUE(pc$plotType %in% availablePlots)
    validDatabase <- isTRUE(pc$database %in% dbTypesRecognised)
    if(!validExpt) {
      flog.error('quickPlot "%s": experiment "%s" not recognised', pc$displayName, pc$experiment)
      invalidExpts <- c(invalidExpts, as.character(pc$experiment))
      validExpt <- FALSE
    }
    if(!validPlotType) {
      flog.error('quickPlot "%s": plotType "%s" not recognised', pc$displayName, pc$plotType)
      invalidPlotNames <- c(invalidPlotNames, as.character(pc$plotType))
    }
    if(!validDatabase) {
      flog.error('quickPlot "%s": database "%s" not recognised', pc$displayName, pc$database)
      invalidDbs <- c(invalidDbs, as.character(pc$database))
    }
    if(!(validExpt && validPlotType && validDatabase)) {
      flog.warn('Failed to initialise quickPlot "%s". It will be ignored', pc$displayName)
      config$quickPlots[[iConfig]] <- NA
      next
    }
    # Process dates
    pc$date <- validateStartDate(pc$date)
    pc$startDate <- validateStartDate(pc$startDate)
    pc$endDate <- validateEndDate(pc$endDate, pc$startDate, pc$nDays)

    # Process chosen obnames
    if(is.null(pc$obs)) {
      allObnames <- getAttrFromMetadata("obname")
      pc$obs <- vector("list", length(allObnames))
      names(pc$obs) <- allObnames
    }
    # Excluding obnames if requested by user
    obsToExclude <- pc$excludeObs
    pc$obs <- pc$obs[!(names(pc$obs) %in% obsToExclude)]
    obnames <- names(pc$obs)

    # Populating variables
    for(iVarList in seq_along(pc$obs)) {
      obname <- obnames[iVarList]
      if(obname=="satem") next # satem obs have no variable lists
      # Each ob will contain a list of variables
      variables <- pc$obs[[iVarList]]
      if(is.null(variables)) {
        allVarsForObname <- getAttrFromMetadata("variables", obname=obname)
        pc$obs[[iVarList]] <- allVarsForObname
      }
    }

    for(obname in obnames) {
      # Satem entries are assumed to have been corectly setup in the config
      # file. The syntax for these is less flexible and require no further
      # parsing. Skipping them.
      if(obname=="satem") next

      # Parsing level and station choices for non-satallite obs.
      # These can either be configured individually for each varname or
      # globally for each obname
      # (i) Parsing level choices
      levelsConfig <- pc$levels[[obname]]
      if(!is.null(levelsConfig) && !is.list(levelsConfig)) {
        # If users set, e.g., "aircraft = 10" for levels in the config file
        pc$levels[[obname]] <- list(allVars=levelsConfig)
      }
      if("allVars" %in% names(pc$levels[[obname]])) {
        # Removing ambiguity if users set "allVars" inside config list
        pc$levels[[obname]][!(names(pc$levels[[obname]])=="allVars")] <- NULL
      }
      # (ii) Parsing station choices
      stationsConfig <- pc$stations[[obname]]
      obtype <- getAttrFromMetadata("category", obname=obname)
      if(plotSupportsChoosingStations(pc$plotType, obtype)) {
        if(!is.null(stationsConfig) && !is.list(stationsConfig)) {
          # If users set, e.g., "aircraft = 10" for levels in the config file
          pc$stations[[obname]] <- list(allVars=stationsConfig)
        }
        if("allVars" %in% names(pc$stations[[obname]])) {
          # Removing ambiguity if users set "allVars" inside config list
          pc$stations[[obname]][!(names(pc$stations[[obname]])=="allVars")] <- NULL
        }
      } else if(!is.null(stationsConfig)) {
        pc$stations[[obname]] <- NULL
        msg <- paste0('quickPlot "',pc$displayName,'": Plot "',pc$plotType,
          '" does not support station choices. Ignoring stations.'
        )
        flog.warn(msg)
      }
    }

    # Save parsed config entry
    config$quickPlots[[iConfig]] <- pc
  }
  config$quickPlots <- Filter(Negate(anyNA), config$quickPlots)


  if(length(invalidExpts)>0) {
    msg <- "OneClick Plots: Please choose your experiment from:"
    for(exptName in exptNamesinConfig) msg <- paste0(msg, "\n  > ", exptName)
    flog.warn(msg)
  }
  if(length(invalidPlotNames)>0) {
    msg <- "OneClick Plots: Please choose your plotType from:"
    for(plotType in availablePlots) msg <- paste0(msg, "\n  > ", plotType)
    flog.warn(msg)
  }
  if(length(invalidDbs)>0) {
    msg <- "OneClick Plots: Please choose your database from:"
    msg <- paste(msg, paste(dbTypesRecognised, collapse=", "))
    flog.warn(msg)
  }

  flog.debug("Finished validation of user-defined quickPlots.")

  return(config)
}

obsmonConfig <<- validateOneClickPlotConfig(obsmonConfig)
