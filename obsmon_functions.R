#
# Help functions and settings for shiny interface
#

default_obtypes     <- c("SYNOP","SHIP","AIRCRAFT","DRIBU","TEMP","SATEM","RADAR")
listOfSensors       <- c("AMSUA","AMSUB","MHS","IASI")

# Normal plots
plotTypesStat       <- c("FG+An departure")
plotTypesMaps       <- c("Observation usage (map)","First guess departure (map)","Analysis departure (map)","Observations (map)")
plotTypesTS         <- c("Number of observations (TS)")
plotTypesSat        <- c("Bias correction (TS)","Hovmoeller (TS)","FG dep + Bias correction (map)","Bias correction (map)")

default_experiments <- c("MetCoOp","Shiny environment","DMI")
#default_experiments <- c("MetCoOp","DMI")

# Predefined plots
preDefinedGroups    <- c("MetCoOp","DMI")

# setExperiment
setExperiment <- function(exp,base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> setExperiment(",exp,base,")"))}
  setExperiment <- NULL
  if ( !is.null(exp)) {
    dbtry_ecma     <- NULL
    dbtry_ecma_sfc <- NULL
    dbtry_ccma     <- NULL
    if ( exp == "MetCoOp" ){
      dbtry_ecma     <- "/data4/portal/metcoop/AM25/archive/extract/ecma/ts/ecma.db"
      dbtry_ecma_sfc <- "/data4/portal/metcoop/AM25/archive/extract/ecma_sfc/ts/ecma.db"
      dbtry_ccma     <- "/data4/portal/metcoop/AM25/archive/extract/ccma/ts/ccma.db"
    } else if ( exp == "Shiny environment" ){
      # Default paths to data bases from environment
      dbtry_ecma     <- paste(Sys.getenv('DBDIR_ECMA'),"/ecma.db",sep="")
      dbtry_ecma_sfc <- paste(Sys.getenv('DBDIR_ECMA_SFC'),"/ecma.db",sep="")
      dbtry_ccma     <- paste(Sys.getenv('DBDIR_CCMA'),"/ccma.db",sep="")
    } else if ( exp == "DMI" ){
      # No paths available yet
      dbtry_ecma     <- "/data4/portal/DMI/EXP/archive/extract/ecma/ts/ecma.db"
      dbtry_ecma_sfc <- "/data4/portal/DMI/EXP/archive/extract/ecma_sfc/ts/ecma.db"
      dbtry_ccma     <- "/data4/portal/DMI/EXP/archive/extract/ccma/ts/ccma.db"
    }

    if ( verbose("DEBUG")) {
      print(paste("DEBUG:     ",exp))
      print(paste("DEBUG:     ",dbtry_ecma))
      print(paste("DEBUG:     ",dbtry_ecma_sfc))
      print(paste("DEBUG:     ",dbtry_ccma))
    }

    # Test if files exist
    if ( base == "Screening" ){
      if ( !is.null(dbtry_ecma) && file.exists(dbtry_ecma)) {
        setExperiment <- dbtry_ecma
      }
    } else if ( base == "Minimization" ){
      if ( !is.null(dbtry_ccma) && file.exists(dbtry_ccma)) {
        setExperiment <- dbtry_ccma
      }
    } else if ( base == "Surface" ){
      if ( !is.null(dbtry_ecma_sfc) && file.exists(dbtry_ecma_sfc)) {
        setExperiment <- dbtry_ecma_sfc
      }
    }

    # Debug
    if (!is.null(setExperiment)){
      if ( verbose("DEBUG")) { print(paste("DEBUG: setExperiment=",setExperiment," base=",base))}
    }
  }
  return (setExperiment)
}

# getExperiments
getExperiments <- function(base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getExperiments(",base,")")) }
  experiments<-NULL
  if ( !is.null(base)){
   for (n in 1:length(default_experiments)){
      if ( !is.null(setExperiment(default_experiments[n],base))){
        experiments=c(experiments,default_experiments[n])
      }
    }
  }
  return (experiments)
}

# obtypeExists
obtypeExists<- function(base,obtype,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> obtypeExists(",base,obtype,daterange,cycle,")")) }
  exists=FALSE
  if (!is.null(base) && !is.null(obtype) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base)
    if ( !is.null(dbConn)) {
     
      query<-paste("SELECT obnumber FROM obsmon WHERE obnumber == ",getObNumber(obtype)," AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," LIMIT 1",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      if ( nrow(dbGetQuery(dbConn,query)) > 0 ) { exists=TRUE}
      disconnect(dbConn)
    } 
  }
  return(exists)
}

# getObtypes
getObtypes <- function(base,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getObtypes(",base,daterange,cycle,")")) }

  if ( !is.null(base)) {
    if ( base == "Surface" ){
      obtypes=c("SYNOP")
    }else{
      obtypes=default_obtypes
    }
    if ( input$showExistingDataOnly ){
      if ( !is.null(daterange) && !is.null(cycle)) {
        obtypes_orig=obtypes
        obtypes=NULL
        for (i in 1:length(obtypes_orig)) {
          if ( obtypeExists(base,obtypes_orig[i],daterange,cycle)) {
            obtypes=c(obtypes,obtypes_orig[i])
          }
        }
      }
    }
    return(obtypes)
  } else {
    return(NULL)
  }
}

# getObNumber
getObNumber <- function(obtype){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getObNumber(",obtype,")")) }
  if ( !is.null(obtype)) {
    switch(obtype,"SYNOP"    = c("1"),
                  "SHIP"     = c("1"),
                  "AIRCRAFT" = c("2"),
                  "DRIBU"    = c("4"),
                  "TEMP"     = c("5"),
                  "SATEM"    = c("7"),
                  "RADAR"    = c("13"),
                  NULL
    )
  }
}

# setDBSatname
setDBSatname<-function(sat){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> setDBSatname(",sat,")")) }
  satname=NULL
  if ( !is.null(sat)) {
    switch(sat,
           "NOAA-15" = {satname="noaa15"},
           "NOAA-16" = {satname="noaa16"},
           "NOAA-17" = {satname="noaa17"},
           "NOAA-18" = {satname="noaa18"},
           "NOAA-19" = {satname="noaa19"},
           "METOP-A" = {satname="metop2"},
           "METOP-B" = {satname="metop1"},
           NULL
    )
  }
  return(satname)
}

# getPlotTypes
getPlotTypes <- function(obtype,dateRange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPlotTypes(",obtype,dateRange,")")) }
  if ( !is.null(obtype)) {
    switch(obtype,"SATEM" = c(plotTypesStat,plotTypesTS,plotTypesMaps,plotTypesSat),c(plotTypesStat,plotTypesMaps,plotTypesTS))
  }
}

# getPlotTypeShort
getPlotTypeShort <- function(plotType){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPlotTypeShort(",plotType,")")) }
  if ( !is.null(plotType)) {
    switch(plotType,
           "FG+An departure"                = "FGAnDeparture",
           "Observation usage (map)"        = "ObservationUsage",
           "Bias correction"                = "BiasCorrection",
           "Hovmoeller"                     = "Hovmoller",
           "First guess departure (map)"    = "FirstGuessDepartureMap",
           "FG dep + Bias correction (map)" = "FirstGuessBCDepartureMap",
           "Analysis departure (map)"       = "AnalysisDepartureMap",
           "Bias correction (map)"          = "BiasCorrectionMap",
           "Observations (map)"             = "ObservationsMap",
           "Number of observations (TS)"    = "NumberOfObservations",
           "Undefined")
  }
}

# getPreDefinedPlots
getPreDefinedGroups <- function(){
  if ( verbose("DEBUG")) { print("DEBUG: -> getPreDefinedGroups") }
  return(preDefinedGroups)
}

# getPreDefinedPlots
getPreDefinedPlots <- function(group){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPreDefinedPlots(",group,")")) }
  if (!is.null(group)){
    switch(group,
               "MetCoOp" = c("Plot 1","Plot 2","Plot 3"),
               "No plots defined!"
   )
  }
}

# variableExists
variableExists<- function(base,obtype,var,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> variableExists(",base,obtype,var,daterange,cycle,")")) }
  exists=FALSE
  if (!is.null(base) && !is.null(obtype) && !is.null(var) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base)
    if ( !is.null(dbConn)) {
       
      query<-paste("SELECT nobs_total FROM obsmon WHERE obnumber == ",getObNumber(obtype)," AND varname == '",var,"' AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," ORDER BY nobs_total DESC LIMIT 1",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      if ( nrow(dbGetQuery(dbConn,query)) > 0 ) { 
        checkData=dbGetQuery(dbConn,query)
        if ( verbose("INFO") ) { print(paste("INFO: nobs_total=",as.character(checkData$nobs_total)))}
        if ( checkData$nobs_total > 0 ) {
          exists=TRUE
        }
      }
      disconnect(dbConn)
    } 
  }
  return(exists)
}

# getVariables
getVariables <- function(obtype,base,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getVariables(",obtype,base,daterange,cycle,")")) }
  if ( !is.null(obtype) && !is.null(base)) {
    if ( base == "Surface" ) {
      synop_vars          <- c("t2m","rh2m","snow")
      ship_vars           <- c("t2m","rh2m")
      dribu_vars          <- c("t2m","rh2m")
    }else{
      synop_vars          <- c("u10m","v10m","z")
      ship_vars           <- c("u10m","v10m","z")
      dribu_vars          <- c("u10m","v10m","z")
    }
    aircraft_vars       <- c("u","v","t")
    temp_vars           <- c("u","v","t","q")
    radar_vars_z        <- c("dbz","radv")
    radar_vars_p        <- c("rh")

    vars=NULL
    switch(obtype, "SYNOP"    = {vars=c(synop_vars)},
                   "SHIP"     = {vars=c(ship_vars)},
                   "AIRCRAFT" = {vars=c(aircraft_vars)},
                   "DRIBU"    = {vars=c(dribu_vars)},
                   "TEMP"     = {vars=c(temp_vars)},
                   "RADAR"    = {vars=c(radar_vars_z,radar_vars_p)},
                   "SATEM"    = {vars=c("rad")},
                   {vars=NULL})

    if ( input$showExistingDataOnly ){
      if ( !is.null(daterange) && !is.null(cycle)) {
        vars_orig=vars
        vars=NULL
        for (i in 1:length(vars_orig)) {
          if ( variableExists(base,obtype,vars_orig[i],daterange,cycle)) {
            vars=c(vars,vars_orig[i])
          }
        }
      }
    }
    return(vars)
  }else{
    return(NULL)
  }
}

# getLevels
getLevels <- function(obtype,var,plotType){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getLevels(",obtype,var,plotType,")")) }
  if ( !is.null(obtype) && !is.null(var)) {
    listOfLevels_p      <- c("100000","92500","80000","60000","45000","35000","27500","22500","17500","12500","8500","6500","4000","2500","1500")
    listOfLevels_z      <- c("250","500","1000","1500","2000","3000","4000","5000","6000","7000","8000","9000","10000","20000")

    # getRadarLevels (Both pressure and z)
    getRadarLevels <- function(var){
      if ( !is.null(var)) {
        switch(var,"rh" = listOfLevels_p,listOfLevels_z)
      }
    }

    all_levels=0
    switch(plotType,"FGAnDeparture" = { all_levels=1})  

    # Set levels
    if ( all_levels == 0 ) {
      switch(obtype, "SYNOP"    = c("Surface"),
                     "SHIP"     = c("Surface"),
                     "AIRCRAFT" = c("ALL",listOfLevels_p),
                     "DRIBU"    = c("Surface"),
                     "TEMP"     = c("ALL",listOfLevels_p),
                     "RADAR"    = c("ALL",getRadarLevels(var)),
                     "Undefined")
     }else{
       switch(obtype,"SYNOP"    = c("Surface"),
                     "SHIP"     = c("Surface"),
                     "AIRCRAFT" = c("ALL"),
                     "DRIBU"    = c("Surface"),
                     "TEMP"     = c("ALL"),
                     "RADAR"    = c("ALL"),
                     "Undefined")
     }
  }
}


# sensorExists
sensorExists<- function(base,sensor,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> sensorExists(",base,sensor,daterange,cycle,")")) }
  exists=FALSE
  if (!is.null(base) && !is.null(sensor) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base)
    if ( !is.null(dbConn)) {

      query<-paste("SELECT nobs_total FROM obsmon WHERE obname == '",tolower(sensor),"' AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," ORDER BY nobs_total DESC LIMIT 1",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      if ( nrow(dbGetQuery(dbConn,query)) > 0 ) {
        checkData=dbGetQuery(dbConn,query)
        if ( verbose("INFO") ) { print(paste("INFO: nobs_total=",as.character(checkData$nobs_total)))}
        if ( checkData$nobs_total > 0 ) {
          exists=TRUE
        }
      }
      disconnect(dbConn)
    }
  }
  return(exists)
}



# getSensors
getSensors <- function(base,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getSensors(",base,daterange,cycle,")")) }
  sensors=listOfSensors
  
  if ( input$showExistingDataOnly ){
    sensors_orig=sensors
    sensors=NULL
    if (!is.null(base) && !is.null(daterange) && !is.null(cycle)){
      for (i in 1:length(sensors_orig)) {
        if ( sensorExists(base,sensors_orig[i],daterange,cycle)) {
          sensors=c(sensors,sensors_orig[i])
        }
      }
    }
  }
  return(sensors)
}

# sateliteExists
sateliteExists<- function(base,sensor,satelite,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> sateliteExists(",base,sensor,satelite,daterange,cycle,")")) }

  exists=FALSE
  if (!is.null(base) && !is.null(sensor) && !is.null(satelite) && !is.null(daterange)){
    dbConn<-connect(base)
    if ( !is.null(dbConn)) {

      query<-paste("SELECT nobs_total FROM obsmon WHERE obname == '",tolower(sensor),"' AND satname == '",setDBSatname(satelite),"' AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," ORDER BY nobs_total DESC LIMIT 1",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      if ( nrow(dbGetQuery(dbConn,query)) > 0 ) {
        checkData=dbGetQuery(dbConn,query)
        if ( verbose("INFO") ) { print(paste("INFO: nobs_total=",as.character(checkData$nobs_total)))}
        if ( checkData$nobs_total > 0 ) {
          exists=TRUE
        }
      }
      disconnect(dbConn)
    }
  }
  return(exists)
}

# getSatelites
getSatelites <- function(base,sensor,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getSatelites(",base,sensor,daterange,cycle,")")) }

  satelites=NULL
  if ( !is.null(base) && !is.null(sensor)) {
    # Set satelites pr. sensor
    switch(sensor, "AMSUA" = {satelites=c("NOAA-15","NOAA-16","NOAA-17","NOAA-18","NOAA-19","METOP-A","METOP-B")},
                   "AMSUB" = {satelites=c("NOAA-15","NOAA-16","NOAA-17","NOAA-18")},
                   "MHS"   = {satelites=c("NOAA-19","METOP-A","METOP-B")},
                   "IASI"  = {satelites=c("METOP-A","METOP-B")},
                   NULL)

    if ( input$showExistingDataOnly ){
      if ( !is.null(daterange) && !is.null(cycle)) {
        satelites_orig=satelites
        satelites=NULL
        for (i in 1:length(satelites_orig)) {
          if ( sateliteExists(base,sensor,satelites_orig[i],daterange,cycle)) {
            satelites=c(satelites,satelites_orig[i])
          }
        }
      }
    }
  }
  return(satelites)
}

# dtg2date
dtg2date <-function(dtg){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> dtg2date",dtg,")")) }

  if ( !is.null(dtg)) {
    date<-paste(substr(dtg,1,4),"-",substr(dtg,5,6),"-",substr(dtg,7,8),sep="")
    return(date)
  }else{
    return(NULL)
  }
}
# dtg2utc
dtg2utc <-function(dtg){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> dtg2utc",dtg,")")) }
  if ( !is.null(dtg)) {
    utc<-paste(substr(dtg,9,10),sep="")
    return(utc)
  }else{
    return(NULL)
  }
}
# date2dtg
date2dtg<-function(date,utc){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> date2dtg",date,utc,")")) }

  if ( !is.null(date) && !is.null(utc) ) {
    dtg=paste(substr(date,1,4),substr(date,6,7),substr(date,9,10),substr(utc,1,2),sep="")
    return(dtg)
  }else{
    return(NULL)
  }
}

# channelExists
channelExists<-  function(base,sensor,sat,channel,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> channelExists",base,sensor,sat,channel,daterange,cycle,")")) }

  exists=FALSE
  if (!is.null(base) && !is.null(sensor) && !is.null(sat) && !is.null(channel) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base)
    if ( !is.null(dbConn)) {

      query<-paste("SELECT nobs_total FROM obsmon WHERE obname == '",tolower(sensor),"' AND satname == '",setDBSatname(sat),"' AND level == ",channel," AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," ORDER BY nobs_total DESC LIMIT 1",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      if ( nrow(dbGetQuery(dbConn,query)) > 0 ) {
        checkData=dbGetQuery(dbConn,query)
        if ( verbose("INFO") ) { print(paste("INFO: nobs_total=",as.character(checkData$nobs_total)))}
        if ( checkData$nobs_total > 0 ) {
          exists=TRUE
        }
      }
      disconnect(dbConn)
    }
  }
  return(exists)
}

# getChannels
getChannels <- function(base,sensor,sat,daterange,cycle){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getChannels",base,sensor,sat,daterange,cycle,")")) }

  channels=NULL
  if ( !is.null(sensor)) {
    listOfChannelsAMSUA <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
    listOfChannelsAMSUB <- c("1","2","3","4","5")
    listOfChannelsMHS   <- c("1","2","3","4","5")
    listOfChannelsIASI  <- c("16","38","49","51","55","57","59","61","63","66","70","72","74","79","81","83","85","87","89","92","95","97","99","101","104","106","109","111","113","116","119","122","125","128","131","133","135","138","141","144","146","148","151","154","157","159","161","163","165","167","170","173","176","178","179","180","183","185","187","189","191","193","195","197","199","201","203","205","207","210","212","214","217","219","222","224","226","228","230","232","234","236","239","241","242","243","246","249","252","254","256","258","260","262","265","267","269","271","272","273","275","278","280","282","284","286","288","290","292","294","296","299","301","303","306","308","310","312","314","316","318","320","323","325","327","329","331","333","335","337","339","341","343","345","347","350","352","354","356","358","360","362","364","366","369","371","373","375","377","379","381","383","386","389","398","401","404","407","410","414","416","426","428","432","434","439","445","457","515","546","552","559","566","571","573","646","662","668","756","867","906","921","1027","1046","1090","1121","1133","1191","1194","1271","1479","1509","1513","1521","1536","1574","1578","1579","1585","1587","1626","1639","1643","1652","1658","1671","1786","1805","1884","1946","1991","2019","2094","2119","2213","2239","2245","2271","2321","2398","2701","2741","2745","2819","2889","2907","2910","2919","2939","2944","2948","2951","2958","2977","2985","2988","2991","2993","3002","3008","3014","3027","3029","3036","3047","3049","3053","3058","3064","3069","3087","3093","3098","3105","3107","3110","3127","3136","3151","3160","3165","3168","3175","3178","3207","3228","3244","3248","3252","3256","3263","3281","3303","3309","3312","3322","3339","3375","3378","3411","3438","3440","3442","3444","3446","3448","3450","3452","3454","3458","3467","3476","3484","3491","3497","3499","3504","3506","3509","3518","3522","3527","3540","3555","3575","3577","3580","3582","3586","3589","3599","3645","3653","3658","3661","3943","4032","5130","5368","5371","5379","5381","5383","5397","5399","5401","5403","5405","5455","5480","5483","5485","5492","5502","5507","5509","5517","5558","5988","5992","5994","6003","6350","6458","6463","6601","6962","6978","6980","6982","6985","6987","6989","6991","6993","6995","6997","7001","7267","7269","7389","7424","7426","7428","7885","8007")

    # Set channels
    switch(sensor, "AMSUA" = {channels=c("ALL",listOfChannelsAMSUA)},
                   "AMSUB" = {channels=c("ALL",listOfChannelsAMSUB)},
                   "MHS"   = {channels=c("ALL",listOfChannelsMHS)},
                   "IASI"  = {channels=c("ALL",listOfChannelsIASI)}
    )

    if ( input$showExistingDataOnly ){
      if ( !is.null(base) && !is.null(sat) && !is.null(daterange) && !is.null(cycle)) {
        channels_orig=channels
        channels=NULL
        if ( length(channels_orig) > 0 ) {
          for (i in 1:length(channels_orig)) {
            if ( channels_orig[i] == "ALL" ){
              channels=c(channels,channels_orig[i])
            }else{
              if ( channelExists(base,sensor,sat,channels_orig[i],daterange,cycle)) {
                channels=c(channels,channels_orig[i])
              }
            }
          }
        }
      }
    }
   }
   return(channels)
}

getUnit<-function(varName){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getUnit(",varName,")")) }
  if ( !is.null(varName)) {
    switch(varName, "u"    = "m/s",
                    "u10m" = "m/s",
                    "v"    = "m/s",
                    "v10m" = "m/s",
                    "t2m"  = "K",
                    "t"    = "K",
                    "q"    = "kg/m3",
                    "z"    = "m",
                    "rh2m" = "%",
                    "snow" = "kg/m2",
                    "rad"  = "K",
                    "")
  }
}


getLatestDate <- function(base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getLatestDate(",base,")")) }
  date<-NULL
  dbConn <- connect(base)
  if ( !is.null(dbConn)){
    plotQuery<-paste("SELECT dtg FROM obsmon ORDER BY dtg DESC LIMIT 1")
    if ( verbose("INFO")) { print(paste("INFO: ",plotQuery)) } 
    plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
    disconnect
    date<-paste(substr(plotData$DTG,1,4),"-",substr(plotData$DTG,5,6),"-",substr(plotData$DTG,7,8),sep="")
  }
  return(date)
}

getLatestCycle <- function(base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getLatestCycle(",base,")")) }
  cycle<-NULL
  dbConn <- connect(base)
  if ( !is.null(dbConn)){
    plotQuery<-paste("SELECT dtg FROM obsmon ORDER BY dtg DESC LIMIT 1")
    if (verbose("INFO")) { print(paste("INFO: ",plotQuery)) }  
    plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
    disconnect
    cycle<-paste(substr(plotData$DTG,9,10),sep="")
  }
  return(cycle)
}

verbose <- function(level){
  verb=FALSE
  if ( !is.null(level)) {
    # Default if level is misspelled is DEBUG
    switch(level,"DEBUG" = {verbositylevel=2}, "INFO" = {verbositylevel=1}, "NONE" = {verbositylevel=0}, {verbositylevel=2})
    if ( !is.null(input$verbosity_chosen)) {
      if ( input$verbosity_chosen == "DEBUG" && verbositylevel <= 2) {
        verb=TRUE
      } else if ( input$verbosity_chosen == "INFO" && verbositylevel <= 1 ){
         verb=TRUE
      }
    } else {
      # Default is verbose as long as input$verbosity is not initialized
      verb=TRUE
    }
  }
  return(verb)
}

# getDumpData
getDumpData<-function(base,table){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: getDumpData(",base,")"))}
  dumpData=NULL
  if ( !is.null(base) && !is.null(table)){
    dbConn=connect(base)
    if (!is.null(dbConn)){
      plotQuery<-paste("SELECT * from ",table,sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
      dumpData <- data.frame(dbGetQuery(dbConn,plotQuery))
      disconnect(dbConn)
    }
  }
  return(dumpData)
}

#
# SQLite and file functions
#

# getFile
getFile <- function(base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getFile(",base,")")) }
  fname<-NULL
  if ( !is.null(base)){
    # Set file name either from experiment description or from uploaded file
    if ( is.null(input$experiment)){ 
      if ( base == "Screening" ){
        if (!is.null(input$ODBbase_screening)){ fname <- input$ODBbase_screening$datapath }
      } else if ( base == "Minimization" ){
        if (!is.null(input$ODBbase_minimization)) { fname <- input$ODBbase_minimization$datapath }
      } else if ( base == "Surface" ){
        if (!is.null(input$ODBbase_surface)) { fname <- input$ODBbase_surface$datapath }
      }
    } else {
      fname<-setExperiment(input$experiment,base)
    }
  }
  return(fname)
}


# connect
connect <- function(base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> connect(",base,")")) }
  dbConn<-NULL
  if ( !is.null(base)) {
    fname <- getFile(base)
    if ( verbose("DEBUG")) { print(paste("DEBUG: -> connect -> ",fname)) }
    if (!is.null(fname) && file.exists(fname)){
      dbConn <- dbConnect("SQLite",fname)
    }
  }
  return(dbConn)
}

# disconnect
disconnect <- function(dbConn){
  if ( verbose("DEBUG")) { print("DEBUG: -> disconnect") }
  if ( !is.null(dbConn)) {
    dbDisconnect(dbConn)
  }
}

