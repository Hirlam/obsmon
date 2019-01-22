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

  availablePlots <- names(plotTypesFlat)
  for(iConfig in seq_along(config$quickPlots)) {
    # pc stands for "plot config"
    pc <- config$quickPlots[[iConfig]]
    if(!isTRUE((pc$experiment %in% exptNamesinConfig))) {
      flog.error('quickPlot "%s": experiment "%s" not recognised', pc$displayName, pc$experiment)
      invalidExpts <- c(invalidExpts, as.character(pc$experiment))
      pc <- NULL
    }
    if(!isTRUE(pc$plotType %in% availablePlots)) {
      flog.error('quickPlot "%s": plotType "%s" not recognised', pc$displayName, pc$plotType)
      invalidPlotNames <- c(invalidPlotNames, as.character(pc$plotType))
      pc <- NULL
    }
    if(!isTRUE((pc$database %in% dbTypesRecognised))) {
      flog.error('quickPlot "%s": database "%s" not recognised', pc$displayName, pc$database)
      invalidDbs <- c(invalidDbs, as.character(pc$database))
      pc <- NULL
    }
    if(is.null(pc)) {
      config$quickPlots[[iConfig]] <- NULL
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
    # Populating variables
    obnames <- names(pc$obs)
    for(iVarList in seq_along(pc$obs)) {
      # Each ob will contain a list of variables
      variables <- pc$obs[[iVarList]]
      if(is.null(variables)) {
        obname <- obnames[iVarList]
        allVarsForObname <- getAttrFromMetadata("variables", obname=obname)
        pc$obs[[iVarList]] <- allVarsForObname
      }
    }
    config$quickPlots[[iConfig]] <- pc
  }

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

  return(config)
}

obsmonConfig <<- validateOneClickPlotConfig(obsmonConfig)
