#!/usr/bin/env Rscript

obSupportsStationChoice <- function(obname=character(0)){
  if(isTRUE(obname %in% c("satem", "aeolus"))) return(FALSE)
  return(TRUE)
}

joinWithDotAsSep <- function(myList) {
  # Joins the elements of myList in a single string, with dots
  # as separators
  rtn <- c()
  for(key in names(myList)) {
    rtn <- c(rtn, paste(key, myList[[key]], sep='.'))
  }
  return(rtn)
}

# Registering general metadata about commonly used observation types
# These are used:
#    (i) To populate fields in the GUI when cache is incomplete or unavailable
#   (ii) To define defaults in multiPlots/batchModePlots
refPressures <- c(
  1500, 2500, 4000, 6500, 8500, 12500, 17500, 22500, 27500, 35000, 45000,
  60000, 80000, 92500, 100000
) %>% set_units("Pa")
refHeights <- c(
  250, 500, 1000, 1500, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000,
  10000, 20000
) %>% set_units("m")

sensorToSats=list(
  amsua=c(
    'noaa15', 'noaa16', 'noaa17', 'noaa18', 'noaa19',
    'metop1', 'metop2', 'metop3'
  ),
  amsub=c(
    'noaa15', 'noaa16', 'noaa17', 'noaa18', 'noaa19',
    'metop1', 'metop2', 'metop3'
  ),
  atms=c('jpss0'),
  iasi=c('metop1', 'metop2', 'metop3'),
  mhs=c('noaa19', 'metop1', 'metop2', 'metop3'),
  mwhs2=c('fy3c', 'fy3d')
)
sensorToSatsScatt=list(
  scatt=c('undefined', 'metopa', 'metopb', 'metopc')
)

generalObsMetadata <- data.frame(
  obname=character(),
  category=character(),
  obnumber=integer(),
  variables=character(),
  sensors.sats=character()
)
registerObservation <- function(dataFrame, ...) {
  newOb <- as.data.frame(list(...))
  allowedCols <- colnames(generalObsMetadata)
  unsetCols <- allowedCols[!(allowedCols %in% colnames(newOb))]
  for(col in unsetCols) newOb[[col]] <- NA
  df <- rbind(dataFrame, newOb)
  return(df)
}

generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='netatmo',
  category='surface',
  obnumber=1,
  variables=c('ps')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='metar',
  category='surface',
  obnumber=1,
  variables=c('z')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='synop',
  category='surface',
  obnumber=1,
  variables=c('apd', 'rh2m', 'snow', 'td2m', 't2m', 'u10m', 'v10m', 'z')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='ship',
  category='surface',
  obnumber=1,
  variables=c('apd', 'rh2m', 'snow', 'td2m', 't2m', 'u10m', 'v10m', 'z')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='aircraft',
  category='upper_air',
  obnumber=2,
  variables=c('t', 'u', 'v')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='amv',
  category='upper_air',
  obnumber=3,
  variables=c('t', 'u', 'v')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='dribu',
  category='surface',
  obnumber=4,
  variables=c('z')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='temp',
  category='upper_air',
  obnumber=5,
  variables=c('q', 't', 'u', 'v')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='pilot',
  category='upper_air',
  obnumber=6,
  variables=c('u', 'v')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='satem',
  category='satem',
  obnumber=7,
  variables=c('rad'),
  sensors.sats=joinWithDotAsSep(sensorToSats)
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='scatt',
  category='scatt',
  obnumber=9,
  variables=c('u10m', 'v10m'),
  sensors.sats=joinWithDotAsSep(sensorToSatsScatt)
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='limb',
  category='upper_air',
  obnumber=10,
  variables=c('bend_angle')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='radar',
  category='radar',
  obnumber=13,
  variables=c('radv', 'dbz', 'rh')
)
generalObsMetadata <- registerObservation(generalObsMetadata,
  obname='aeolus',
  category='lidar',
  obnumber=15,
  variables=c('hlos')
)

# Function to extract info from generalObsMetadata
getAttrFromMetadata <- function(attr, ...) {
  attr <- tolower(attr)
  optArgs <- list(...)
  if(length(optArgs)>1) {
    stop('getAttrFromMetadata: Only 1 optional arg supported.')
  } else if(length(optArgs)==1) {
    attrValues <- generalObsMetadata[
      which(generalObsMetadata[[names(optArgs)[1]]]==optArgs[[1]]),
      attr
    ]
  } else {
    attrValues <- generalObsMetadata[, attr]
  }
  rtn <- sort(unique(as.vector(attrValues)))
  if(length(rtn)==0) rtn <- NA
  return(rtn)
}
# Useful wrappers
getDefaultsForObname <- function(obname) {
  rtn <- list(
    variables <- getAttrFromMetadata('variables', obname=obname)
  )
}
getSensorNamesFromMetadata <- function() {
  sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
  rtn <- gsub('\\.{1}.*', '', sens.sats)
  return(sort(unique(rtn)))
}
getSatelliteNamesFromMetadata <- function(sensor) {
  sens.sats <- getAttrFromMetadata('sensors.sats', category="satem")
  sens.sats <- sens.sats[startsWith(sens.sats, paste0(sensor, '.'))]
  rtn <- gsub(paste0(sensor, '.'), '', sens.sats, fixed=TRUE)
  return(sort(unique(rtn)))
}
getScattSatnamesFromMetadata <- function() {
  sens.sats <- getAttrFromMetadata('sensors.sats', category="scatt")
  sens.sats <- sens.sats[startsWith(sens.sats, paste0('scatt', '.'))]
  rtn <- gsub(paste0('scatt', '.'), '', sens.sats, fixed=TRUE)
  return(sort(unique(rtn)))
}


# Testing. This is executed only if the script is run directly
if(sys.nframe()==0){
  # Retrieve all categories registered in the generalObsMetadata dataframe
  obsCategories <- getAttrFromMetadata('category')
  for(categ in obsCategories) {
    obnames <- getAttrFromMetadata('obname', category=categ)
    msg <- paste0('category ', categ, ', ')
    msg <- paste0(msg, 'obnames=(', paste(obnames, collapse=', '), ')\n')
    cat(msg)
  }
  cat('\n')
  # Retrieve all obnames registered in the generalObsMetadata dataframe
  obnames <- getAttrFromMetadata('obname')
  for(obname in obnames) {
    variables <- getAttrFromMetadata('variables', obname=obname)
    obnumber <- getAttrFromMetadata('obnumber', obname=obname)
    categ <- getAttrFromMetadata('category', obname=obname)
    msg <- paste0('obname: ', obname, '\n')
    msg <- paste0(msg, '  > obnumber=', obnumber, '\n')
    msg <- paste0(msg, '  > category=', categ, '\n')
    msg <- paste0(msg,'  > variables=(',paste(variables, collapse=', '),')\n')
    cat(msg)
  }
  for(categ in obsCategories) {
    cat(categ, ': ', getAttrFromMetadata('obnumber', category=categ), '\n')
  }
}
