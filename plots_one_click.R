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
  anyInvalidExpt <- FALSE
  anyInvalidPlotType <- FALSE

  availablePlots <- names(plotTypesFlat)
  availableExpts <- c()
  for(expt in config$experiments) availableExpts <- c(availableExpts, expt$displayName)
  for(iPlotConfig in length(config$oneClickPlots)) {
    # pc stands for "plot config"
    pc <- config$oneClickPlots[[iPlotConfig]]
    validExpt <- pc$experiment %in% availableExpts
    validPlotType <- pc$plotType %in% availablePlots
    validOneClickPlot <- validExpt && validPlotType
    # Process dates
    pc$date <- validateStartDate(pc$date)
    pc$startDate <- validateStartDate(pc$startDate)
    pc$endDate <- validateEndDate(pc$endDate, pc$startDate, pc$nDays)

    # TODO: Validate database (ecma, ccma, ecma_sfc)

    if(!validOneClickPlot) {
      msg <- sprintf('OneClick plots: Ignoring "%s" -- Invalid configuration', pc$displayName)
      if(!validExpt) {
        anyInvalidExpt <- TRUE
        msg <- sprintf('%s\n   > Experiment "%s" not found', msg, pc$experiment)
      }
      if(!validPlotType) {
        anyInvalidPlotType <- TRUE
        msg <- sprintf('%s\n   > Plot type "%s" not found', msg, pc$plotType)
      }
      config$oneClickPlots[iPlotConfig] <- NULL
      flog.error(msg)
    }

    # Saving changes
    config$oneClickPlots[[iPlotConfig]] <- pc
  }
  if(anyInvalidExpt) {
    msg <- "OneClick Plots: Please choose your experiment from:"
    for(exptName in availableExpts) msg <- paste0(msg, "\n  > ", exptName)
    flog.warn(msg)
  }
  if(anyInvalidPlotType) {
    msg <- "OneClick Plots: Please choose your plotType from:"
    for(plotType in availablePlots) msg <- paste0(msg, "\n  > ", plotType)
    flog.warn(msg)
  }
  return(config)
}

obsmonConfig <<- validateOneClickPlotConfig(obsmonConfig)
