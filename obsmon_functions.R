#
# Help functions and settings for shiny interface
#

# Check installation
exp1=""
exp2=""
# hirlam.org
if ( Sys.info()["nodename"] == "hirlam" ){
  hostname="hirlam"
  default_experiments <- c("MEPS-mbr000","DMI","FMI","MEPS-mbr001","MEPS-preop","DMI-dka38h12","DMI-dka38h12b","AROME-Arctic","IGA")
  for ( m in 2:9){
    default_experiments <- c(default_experiments,paste("MEPS-mbr00",m,sep=""))
  }
  default_experiments <- c(default_experiments,paste("MetCoOp (old)","MetCoOp-backup (old)","MetCoOp-preop (old)",sep=""))
# MetCoOp server
}else if (Sys.getenv('SMHI_DIST') == "elin3" | Sys.getenv('SMHI_DIST') == "elin4" ) {
  hostname="metcoop"
  default_experiments <- c("MEPS-mbr000","MEPS-mbr001")
  default_experiments <- c(default_experiments,paste("MEPS-preop",sep=""))
  for ( m in 2:9){
    default_experiments <- c(default_experiments,paste("MEPS-mbr00",m,sep=""))
  }
  default_experiments <- c(default_experiments,paste("MetCoOp (old)","MetCoOp-backup (old)","MetCoOp-preop (old)",sep=""))
# Default
}else{
  hostname="default"
  exp1 <- Sys.getenv('OBSMON_EXP1', unset = "exp1")
  exp2 <- Sys.getenv('OBSMON_EXP2', unset = "exp2")
  default_experiments <- c(exp1,exp2)
}

default_obtypes     <- c("SYNOP","SHIP","AIRCRAFT","DRIBU","TEMP","LIMB","AMV","SATEM","SCATT","RADAR")
listOfSensors       <- c("AMSUA","AMSUB","MHS","ATMS","IASI")

# Normal plots
plotTypesStat       <- c("FG+An departure")
plotTypesMaps       <- c("Observation usage (map)","First guess departure (map)","Analysis departure (map)","Analysis increment (map)","Observations (map)")
plotTypesTS         <- c("Number of observations (TS)","Obs fit (TS)")
plotTypesSat        <- c("FG dep + Bias correction (map)","Bias correction (map)")
plotTypesSatTS      <- c("Bias correction (TS)","Land-sea departures (TS)","Hovmoeller (TS)")

# setExperiment
setExperiment <- function(exp,base,dtg=NULL,dir=F){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> setExperiment(",exp,base,dtg,dir,")"))}
  setExperiment=NULL

  if ( !is.null(exp)) {
    dbtry_ecma     <- NULL
    dbtry_ecma_sfc <- NULL
    dbtry_ccma     <- NULL
    if ( exp == "MetCoOp (old)" ){
      if ( hostname == "hirlam" ){
        dbtry_ecma     =  "/data4/portal/metcoop/AM25/archive/extract/ecma/"
        dbtry_ecma_sfc =  "/data4/portal/metcoop/AM25/archive/extract/ecma_sfc/"
        dbtry_ccma     =  "/data4/portal/metcoop/AM25/archive/extract/ccma/"
      }else if ( hostname == "metcoop" ){
        dbtry_ecma     =  "/nobackup/opdata/obsmon_main/ecma/"
        dbtry_ecma_sfc =  "/nobackup/opdata/obsmon_main/ecma_sfc/"
        dbtry_ccma     =  "/nobackup/opdata/obsmon_main/ccma/"
      }
    } else if ( exp == "MetCoOp-backup (old)" ){
      if ( hostname == "hirlam" ){
        dbtry_ecma     =  "/data4/portal/metcoop/AM25_backup/archive/extract/ecma/"
        dbtry_ecma_sfc =  "/data4/portal/metcoop/AM25_backup/archive/extract/ecma_sfc/"
        dbtry_ccma     =  "/data4/portal/metcoop/AM25_backup/archive/extract/ccma/"
      }else if ( hostname == "metcoop" ){
        dbtry_ecma     =  "/nobackup/opdata/obsmon_backup/ecma/"
        dbtry_ecma_sfc =  "/nobackup/opdata/obsmon_backup/ecma_sfc/"
        dbtry_ccma     =  "/nobackup/opdata/obsmon_backup/ccma/"
      }
    } else if ( exp == "MetCoOp-preop (old)" ){
      if ( hostname == "hirlam" ){
        dbtry_ecma     =  "/data4/portal/metcoop/AM25_preop/archive/extract/ecma/"
        dbtry_ecma_sfc =  "/data4/portal/metcoop/AM25_preop/archive/extract/ecma_sfc/"
        dbtry_ccma     =  "/data4/portal/metcoop/AM25_preop/archive/extract/ccma/"
      }else if ( hostname == "metcoop" ){
        dbtry_ecma     =  "/nobackup/opdata/obsmon_preop/ecma/"
        dbtry_ecma_sfc =  "/nobackup/opdata/obsmon_preop/ecma_sfc/"
        dbtry_ccma     =  "/nobackup/opdata/obsmon_preop/ccma/"
      }
    # MEPS
    } else if ( exp == "MEPS-mbr000" || exp == "MEPS-mbr001" || exp == "MEPS-mbr002" || exp == "MEPS-mbr003" || exp == "MEPS-mbr004" || exp == "MEPS-mbr005" || exp == "MEPS-mbr006" || exp == "MEPS-mbr007" || exp == "MEPS-mbr008" || exp == "MEPS-mbr009" ) {
      if ( hostname == "hirlam" ){
        obsmon_root="/data4/portal/metcoop/MEPS_prod/archive/obsmon"
      }else if ( hostname == "metcoop" ){
        obsmon_root="/nobackup/opdata/meps/obsmon/"
      }
      dbtry_ecma     =  paste(obsmon_root,"/",substring(exp,6,11),"/ecma/",sep="")
      dbtry_ecma_sfc =  paste(obsmon_root,"/",substring(exp,6,11),"/ecma_sfc/",sep="")
      dbtry_ccma     =  paste(obsmon_root,"/",substring(exp,6,11),"/ccma/",sep="")
    } else if ( exp == "MEPS-preop" ) {
      if ( hostname == "hirlam" ){
        obsmon_root="/data4/portal/metcoop/MEPS_preop/archive/obsmon/mbr000/"
      }else if ( hostname == "metcoop" ){
        obsmon_root="/nobackup/opdata/meps_preop/obsmon/mbr000/"
      }
      dbtry_ecma     =  paste(obsmon_root,"/ecma/",sep="")
      dbtry_ecma_sfc =  paste(obsmon_root,"/ecma_sfc/",sep="")
      dbtry_ccma     =  paste(obsmon_root,"/ccma/",sep="")
    } else if ( exp == exp1 ){
      # Default paths to data bases from environment
      dbtry_ecma     =  Sys.getenv('DBDIR_ECMA')
      dbtry_ecma_sfc =  Sys.getenv('DBDIR_ECMA_SFC')
      dbtry_ccma     =  Sys.getenv('DBDIR_CCMA')
    } else if ( exp == exp2 ){
      # Default paths to data bases from environment
      dbtry_ecma     =  Sys.getenv('DBDIR_ECMA2')
      dbtry_ecma_sfc =  Sys.getenv('DBDIR_ECMA_SFC2')
      dbtry_ccma     =  Sys.getenv('DBDIR_CCMA2')
    } else if ( exp == "DMI" ){
      dbtry_ecma     =  "/data4/portal/dmi/dka38h12/archive/extract/ecma/"
      dbtry_ecma_sfc =  "/data4/portal/dmi/dka38h12/archive/extract/ecma_sfc/"
      dbtry_ccma     =  "/data4/portal/dmi/dka38h12/archive/extract/ccma/"
    } else if ( exp == "IGA" ){
      dbtry_ecma     = "/data4/portal/dmi/IGA/archive/extract/ecma/"
      dbtry_ecma_sfc = "/data4/portal/dmi/IGA/archive/extract/ecma_sfc/"
      dbtry_ccma     = "/data4/portal/dmi/IGA/archive/extract/ccma"
    } else if ( exp == "DMI-dka38h12" ){
      dbtry_ecma     =  "/data4/portal/dmi/dka38h12/archive/extract/ecma/"
      dbtry_ecma_sfc =  "/data4/portal/dmi/dka38h12/archive/extract/ecma_sfc/"
      dbtry_ccma     =  "/data4/portal/dmi/dka38h12/archive/extract/ccma/"
    } else if ( exp == "DMI-dka38h12b" ){
      dbtry_ecma     =  "/data4/portal/dmi/dka38h12b/archive/extract/ecma/"
      dbtry_ecma_sfc =  "/data4/portal/dmi/dka38h12b/archive/extract/ecma_sfc/"
      dbtry_ccma     =  "/data4/portal/dmi/dka38h12b/archive/extract/ccma/"
    } else if ( exp == "FMI" ){
      dbtry_ecma     =  "/data4/portal/fmi/aro38h12/extract/ecma/"
      dbtry_ecma_sfc =  "/data4/portal/fmi/aro38h12/extract/ecma_sfc/"
      dbtry_ccma     =  "/data4/portal/fmi/aro38h12/extract/ccma/"
    } else if ( exp == "AROME-Arctic" ){
      dbtry_ecma     =  "/data4/portal/metno/AROME_Arctic/archive/extract/ecma/"
      dbtry_ecma_sfc =  "/data4/portal/metno/AROME_Arctic/archive/extract/ecma_sfc/"
      dbtry_ccma     =  "/data4/portal/metno/AROME_Arctic/archive/extract/ccma/"
    }

    # Keep directories if no DTG is set
    if ( !dir ) {
      if ( is.null(dtg)) { print(paste("WARNING: dtg is not set and should be set")) }
      dbtry_ecma     = paste(dbtry_ecma,"/",dtg,"/ecma.db",sep="")
      dbtry_ecma_sfc = paste(dbtry_ecma_sfc,"/",dtg,"/ecma.db",sep="")
      dbtry_ccma     = paste(dbtry_ccma,"/",dtg,"/ccma.db",sep="")
    }
    if ( verbose("DEBUG")) {
      print(paste("DEBUG:     ",exp))
      print(paste("DEBUG:     ",dbtry_ecma))
      print(paste("DEBUG:     ",dbtry_ecma_sfc))
      print(paste("DEBUG:     ",dbtry_ccma))
    }

    if ( base == "Screening" ){
      if ( dir ) { 
        setExperiment = dbtry_ecma
      } else{
        if ( !is.null(dbtry_ecma) ) {
          dtg=getLatestDTG(base,exp)
          if ( !is.null(dtg)) { setExperiment = dbtry_ecma }
        }
      }
    } else if ( base == "Minimization" ){
      if ( dir ) { 
        setExperiment = dbtry_ccma
      }else{
        if ( !is.null(dbtry_ccma) ) {
          dtg=getLatestDTG(base,exp)
          if ( !is.null(dtg)) { setExperiment = dbtry_ccma }
        }
      }
    } else if ( base == "Surface" ){
      if ( dir ) { 
        setExperiment = dbtry_ecma_sfc 
      }else{
        if ( !is.null(dbtry_ecma_sfc) ) {
          dtg=getLatestDTG(base,exp)
          if ( !is.null(dtg)) { setExperiment = dbtry_ecma_sfc }
        }
      }
    }

    # Debug
    if (!is.null(setExperiment)){
      if ( verbose("DEBUG")) { print(paste("DEBUG: setExperiment=",setExperiment," base=",base,sep=""))}
    }else{
      if ( verbose("DEBUG")) { print("DEBUG: setExperiment=NULL") }
    }
  }
  return (setExperiment)
}

# getExperiments
getExperiments <- function(base,dtg=NULL){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getExperiments(",base,dtg,")")) }
  experiments=NULL

  if ( !is.null(base)){
   for (n in 1:length(default_experiments)){
      if ( !is.null(setExperiment(default_experiments[n],base,dtg))){
        experiments=c(experiments,default_experiments[n])
      }
    }
  }
  return (experiments)
}

# obtypeExists
obtypeExists<- function(obtype){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> obtypeExists(",obtype,")")) }
  exists=FALSE

  base=NULL
  daterange=NULL
  cycle=NULL
  exp=NULL
  if ( input$tabs == "Surface" ){
    base="Surface"
    if ( !is.null(input$dateRange_SA)){ daterange=input$dateRange_SA }
    if ( !is.null(input$cycle_SA)){ cycle=input$cycle_SA }
    if ( !is.null(input$experiment_SA)){ exp=input$experiment_SA }
  }else{
    if ( !is.null(input$ODBbase)){ base=input$ODBbase }
    if ( !is.null(input$dateRange)){ daterange=input$dateRange }
    if ( !is.null(input$cycle)){ cycle=input$cycle }
    if ( !is.null(input$experiment)){ exp=input$experiment }  
  }

  if (!is.null(base) && !is.null(exp) && !is.null(obtype) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base,exp,date2dtg(daterange[1],cycle))
    if ( !is.null(dbConn)) {
     
      query<-paste("SELECT nobs_total FROM obsmon WHERE obnumber == ",getObNumber(obtype)," AND DTG >= ",date2dtg(daterange[1],cycle)," AND DTG <= ",date2dtg(daterange[2],cycle)," ORDER BY nobs_total DESC LIMIT 1",sep="")
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

# getObtypes
getObtypes <- function(){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getObtypes")) }

  obtypes=NULL
  if ( input$tabs == "Surface" ){
    obtypes=c("SYNOP")
  }else{
    obtypes=default_obtypes
  }
  if ( input$showExistingDataOnly ){
    obtypes_orig=obtypes
    obtypes=NULL
    for (i in 1:length(obtypes_orig)) {
      if ( obtypeExists(obtypes_orig[i])) {
        obtypes=c(obtypes,obtypes_orig[i])
      }
    }
  }
  return(obtypes)
}

# getObNumber
getObNumber <- function(obtype){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getObNumber(",obtype,")")) }
  if ( !is.null(obtype)) {
    switch(obtype,"SYNOP"    = c("1"),
                  "SHIP"     = c("1"),
                  "AIRCRAFT" = c("2"),
                  "AMV"      = c("3"),
                  "DRIBU"    = c("4"),
                  "TEMP"     = c("5"),
                  "SATEM"    = c("7"),
                  "SCATT"    = c("9"),
                  "LIMB"     = c("10"),
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
           "JPSS0"   = {satname="jpss0"},
           NULL
    )
  }
  return(satname)
}

# getPlotTypes
getPlotTypes <- function(obtype,base){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPlotTypes(",obtype,base,")")) }
  if ( !is.null(obtype) && !is.null(base) ) {
    switch(obtype,"SATEM" = c(plotTypesStat,plotTypesMaps,plotTypesSat,plotTypesTS,plotTypesSatTS),c(plotTypesStat,plotTypesMaps,plotTypesTS))
  }
}

# getPlotTypeShort
getPlotTypeShort <- function(plotType){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPlotTypeShort(",plotType,")",sep="")) }
  if ( !is.null(plotType)) {
    switch(plotType,
           "FG+An departure"                = "FGAnDeparture",
           "Observation usage (map)"        = "ObservationUsage",
           "Bias correction (TS)"           = "BiasCorrection",
           "Land-sea departures (TS)"       = "LandSeaDepartures",
           "Hovmoeller (TS)"                = "Hovmoller",
           "First guess departure (map)"    = "FirstGuessDepartureMap",
           "FG dep + Bias correction (map)" = "FirstGuessBCDepartureMap",
           "Analysis departure (map)"       = "AnalysisDepartureMap",
           "Analysis increment (map)"       = "AnalysisIncrementMap",
           "Bias correction (map)"          = "BiasCorrectionMap",
           "Observations (map)"             = "ObservationsMap",
           "Number of observations (TS)"    = "NumberOfObservations",
           "Obs fit (TS)"                   = "ObsFitTs",
           plotType)
  }
}

# variableExists
variableExists<- function(obtype,var){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> variableExists(",obtype,var,")")) }
  exists=FALSE

  if ( !is.null(var)){
    if ( var == "ff" ){
      var="u"
    }
    if ( var == "ff10m" ){
      var="u10m"
    }
  }
  base=NULL
  daterange=NULL
  cycle=NULL
  exp=NULL
  if ( input$tabs == "Surface" ){
    base="Surface"
    if ( !is.null(input$dateRange_SA)){ daterange=input$dateRange_SA }
    if ( !is.null(input$cycle_SA)){ cycle=input$cycle_SA }
    if ( !is.null(input$experiment_SA)){ exp=input$experiment_SA }
  }else{
    if ( !is.null(input$ODBbase)){ base=input$ODBbase }
    if ( !is.null(input$dateRange)){ daterange=input$dateRange }
    if ( !is.null(input$cycle)){ cycle=input$cycle }
    if ( !is.null(input$experiment)){ exp=input$experiment }
  }

  if (!is.null(base) && !is.null(exp) && !is.null(obtype) && !is.null(var) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base,exp,date2dtg(daterange[1],cycle))
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
getVariables <- function(obtype){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getVariables(",obtype,")")) } 
  if ( !is.null(obtype)) {
    if ( input$tabs == "Surface" ) {
      synop_vars          <- c("t2m","rh2m","snow")
      ship_vars           <- c("t2m","rh2m")
      dribu_vars          <- c("t2m","rh2m")
    }else{
      synop_vars          <- c("u10m","v10m","ff10m","z","apd")
      ship_vars           <- c("u10m","v10m","ff10m","z")
      dribu_vars          <- c("u10m","v10m","ff10m","z")
    }
    scatt_vars          <- c("u10m","v10m","ff10m")
    aircraft_vars       <- c("u","v","ff","t")
    amv_vars            <- c("u","v","t")
    temp_vars           <- c("u","v","ff","t","q")
    radar_vars_z        <- c("dbz","radv")
    radar_vars_p        <- c("rh")
    limb_vars           <- c("bend_angle")

    vars=NULL
    switch(obtype, "SYNOP"    = {vars=c(synop_vars)},
                   "SHIP"     = {vars=c(ship_vars)},
                   "AIRCRAFT" = {vars=c(aircraft_vars)},
                   "DRIBU"    = {vars=c(dribu_vars)},
                   "TEMP"     = {vars=c(temp_vars)},
                   "AMV"      = {vars=c(amv_vars)},
                   "SCATT"    = {vars=c(scatt_vars)},
                   "RADAR"    = {vars=c(radar_vars_z,radar_vars_p)},
                   "LIMB"     = {vars=c(limb_vars)},
                   {vars=NULL})

    if ( input$showExistingDataOnly ){
      vars_orig=vars
      vars=NULL
      for (i in 1:length(vars_orig)) {
        if ( variableExists(obtype,vars_orig[i])) {
          vars=c(vars,vars_orig[i])
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
  if ( !is.null(obtype) && !is.null(var) && !is.null(plotType)) {
    #listOfLevels_p      <- c("100000","92500","85000","70000","50000","40000","30000","25000","20000","15000","10000","8500","6500","4000","2500","1500")
    # 1500,2500,4000,6500,8500,12500,17500,22500,27500,35000,45000,60000,80000,92500,100000
    listOfLevels_p      <- c("100000","92500","80000","60000","45000","35000","27500","22500","17500","12500","8500","4000","2500","1500")
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
                     "AMV"      = c("ALL",listOfLevels_p),
                     "DRIBU"    = c("Surface"),
                     "TEMP"     = c("ALL",listOfLevels_p),
                     "RADAR"    = c("ALL",getRadarLevels(var)),
                     "LIMB"     = c("ALL",listOfLevels_p),
                     "SCATT"    = c("Surface"),
                     NULL)
     }else{
       switch(obtype,"SYNOP"    = c("Surface"),
                     "SHIP"     = c("Surface"),
                     "AIRCRAFT" = c("ALL"),
                     "AMV"      = c("ALL"),
                     "LIMB"     = c("ALL"),
                     "DRIBU"    = c("Surface"),
                     "TEMP"     = c("ALL"),
                     "RADAR"    = c("ALL"),
                     "SCATT"    = c("Surface"),
                     NULL)
     }
  }else{
    if ( verbose("DEBUG")) { print("DEBUG: -> getLevels: Not all needed arguments set") }
    return(NULL)
  }
}


# sensorExists
sensorExists<- function(sensor){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> sensorExists(",sensor,")")) }
  exists=FALSE

  base=NULL
  daterange=NULL
  cycle=NULL
  exp=NULL
  if ( !is.null(input$ODBbase)){ base=input$ODBbase }
  if ( !is.null(input$dateRange)){ daterange=input$dateRange }
  if ( !is.null(input$cycle)){ cycle=input$cycle }
  if ( !is.null(input$experiment)){ exp=input$experiment }

  if (!is.null(base) && !is.null(exp) && !is.null(sensor) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base,exp,date2dtg(daterange[1],cycle))
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
getSensors <- function(){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getSensors")) }
  sensors=listOfSensors
  
  if ( input$showExistingDataOnly ){
    sensors_orig=sensors
    sensors=NULL
    if (!is.null(sensors_orig)){
      for (i in 1:length(sensors_orig)) {
        if ( sensorExists(sensors_orig[i])) {
          sensors=c(sensors,sensors_orig[i])
        }
      }
    }
  }
  return(sensors)
}

# sateliteExists
sateliteExists<- function(sensor,satelite){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> sateliteExists(",sensor,satelite,")")) }

  exists=FALSE
  base=NULL
  exp=NULL
  daterange=NULL
  cycle=NULL
  if ( input$tabs == "Surface" ){
    base="Surface"
    if ( !is.null(input$dateRange_SA)){ daterange=input$dateRange_SA }
    if ( !is.null(input$cycle_SA)){ cycle=input$cycle_SA }
    if ( !is.null(input$experiment_SA)){ exp=input$experiment_SA }
  }else{
    if ( !is.null(input$ODBbase)){ base=input$ODBbase }
    if ( !is.null(input$dateRange)){ daterange=input$dateRange }
    if ( !is.null(input$cycle)){ cycle=input$cycle }
    if ( !is.null(input$experiment)){ exp=input$experiment }
  }

  if (!is.null(base) && !is.null(exp) && !is.null(sensor) && !is.null(satelite) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base,exp,date2dtg(daterange[1],cycle))
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
getSatelites <- function(sensor){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getSatelites(",sensor,")")) }

  satelites=NULL
  if ( !is.null(sensor)) {
    # Set satelites pr. sensor
    switch(sensor, "AMSUA" = {satelites=c("NOAA-15","NOAA-16","NOAA-17","NOAA-18","NOAA-19","METOP-A","METOP-B")},
                   "AMSUB" = {satelites=c("NOAA-15","NOAA-16","NOAA-17","NOAA-18")},
                   "MHS"   = {satelites=c("NOAA-19","METOP-A","METOP-B")},
                   "ATMS"  = {satelites=c("JPSS0")},
                   "IASI"  = {satelites=c("METOP-A","METOP-B")},
                   NULL)

    if ( input$showExistingDataOnly ){
      satelites_orig=satelites
      satelites=NULL
      if ( !is.null(satelites_orig)) {
        for (i in 1:length(satelites_orig)) {
          if ( sateliteExists(sensor,satelites_orig[i])) {
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
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> date2dtg(",date,utc,")")) }

  if ( !is.null(date) && !is.null(utc) ) {
    dtg=paste(substr(date,1,4),substr(date,6,7),substr(date,9,10),substr(utc,1,2),sep="")
    return(dtg)
  }else{
    return(NULL)
  }
}

# channelExists
channelExists<-  function(sensor,sat,channel){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> channelExists",sensor,sat,channel,")")) }

  exists=FALSE
  base=NULL
  exp=NULL
  daterange=NULL
  cycle=NULL
  if ( !is.null(input$ODBbase)){ base=input$ODBbase }
  if ( !is.null(input$dateRange)){ daterange=input$dateRange }
  if ( !is.null(input$cycle)){ cycle=input$cycle }
  if ( !is.null(input$experiment)){ exp=input$experiment }

  if (!is.null(base) && !is.null(exp) && !is.null(sensor) && !is.null(sat) && !is.null(channel) && !is.null(daterange) && !is.null(cycle)){
    dbConn<-connect(base,exp,date2dtg(daterange[1],cycle))
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
getChannels <- function(sensor,sat){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getChannels",sensor,sat,")")) }

  channels=NULL
  if ( !is.null(sensor)) {
    listOfChannelsAMSUA <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
    listOfChannelsAMSUB <- c("1","2","3","4","5")
    listOfChannelsATMS <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22")
    listOfChannelsMHS   <- c("1","2","3","4","5")
    #listOfChannelsIASI  <- c("16","38","49","51","55","57","59","61","63","66","70","72","74","79","81","83","85","87","89","92","95","97","99","101","104","106","109","111","113","116","119","122","125","128","131","133","135","138","141","144","146","148","151","154","157","159","161","163","165","167","170","173","176","178","179","180","183","185","187","189","191","193","195","197","199","201","203","205","207","210","212","214","217","219","222","224","226","228","230","232","234","236","239","241","242","243","246","249","252","254","256","258","260","262","265","267","269","271","272","273","275","278","280","282","284","286","288","290","292","294","296","299","301","303","306","308","310","312","314","316","318","320","323","325","327","329","331","333","335","337","339","341","343","345","347","350","352","354","356","358","360","362","364","366","369","371","373","375","377","379","381","383","386","389","398","401","404","407","410","414","416","426","428","432","434","439","445","457","515","546","552","559","566","571","573","646","662","668","756","867","906","921","1027","1046","1090","1121","1133","1191","1194","1271","1479","1509","1513","1521","1536","1574","1578","1579","1585","1587","1626","1639","1643","1652","1658","1671","1786","1805","1884","1946","1991","2019","2094","2119","2213","2239","2245","2271","2321","2398","2701","2741","2745","2819","2889","2907","2910","2919","2939","2944","2948","2951","2958","2977","2985","2988","2991","2993","3002","3008","3014","3027","3029","3036","3047","3049","3053","3058","3064","3069","3087","3093","3098","3105","3107","3110","3127","3136","3151","3160","3165","3168","3175","3178","3207","3228","3244","3248","3252","3256","3263","3281","3303","3309","3312","3322","3339","3375","3378","3411","3438","3440","3442","3444","3446","3448","3450","3452","3454","3458","3467","3476","3484","3491","3497","3499","3504","3506","3509","3518","3522","3527","3540","3555","3575","3577","3580","3582","3586","3589","3599","3645","3653","3658","3661","3943","4032","5130","5368","5371","5379","5381","5383","5397","5399","5401","5403","5405","5455","5480","5483","5485","5492","5502","5507","5509","5517","5558","5988","5992","5994","6003","6350","6458","6463","6601","6962","6978","6980","6982","6985","6987","6989","6991","6993","6995","6997","7001","7267","7269","7389","7424","7426","7428","7885","8007")
    listOfChannelsIASI  <- c("38","51","63","85","104","109","167","173","180","185","193","199","205","207","212","224","230","236","239","242","243","249","296","333","337","345","352","386","389","432","2701","2819","2910","2919","2991","2993","3002","3008","3014","3098","3207","3228","3281","3309","3322","3438","3442","3484","3491","3499","3506","3575","3582","3658","4032")


    # Set channels
    switch(sensor, "AMSUA" = {channels=c(listOfChannelsAMSUA)},
                   "AMSUB" = {channels=c(listOfChannelsAMSUB)},
                   "MHS"   = {channels=c(listOfChannelsMHS)},
                   "ATMS"  = {channels=c(listOfChannelsATMS)},
                   "IASI"  = {channels=c(listOfChannelsIASI)}
    )

    if ( input$showExistingDataOnly ){
      channels_orig=channels
      channels=NULL
      if ( !is.null(channels_orig)) {
        if ( length(channels_orig) > 0 ) {
          for (i in 1:length(channels_orig)) {
            if ( channels_orig[i] == "ALL" ){
              channels=c(channels,channels_orig[i])
            }else{
              # Disable checking for IASI as this is huge!
              if ( sensor == "IASI" ){
                channels=c(channels,channels_orig[i])
              }else{
                if ( channelExists(sensor,sat,channels_orig[i])){
                  channels=c(channels,channels_orig[i])
                }
              }
            }
          }
        }
      }
    }
   }
   return(channels)
}

# getUnit
getUnit<-function(varName){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getUnit(",varName,")")) }
  if ( !is.null(varName)) {
    switch(varName, "u"    = "m/s",
                    "ff"   = "m/s",
                    "u10m" = "m/s",
                    "ff10m"= "m/s",
                    "apd"  = "m",
                    "v"    = "m/s",
                    "v10m" = "m/s",
                    "t2m"  = "K",
                    "t"    = "K",
                    "q"    = "kg/m3",
                    "z"    = "m",
                    "rh2m" = "%",
                    "snow" = "kg/m2",
                    "rad"  = "K",
                    "radv" = "m/s",
                    "dbz"  = "db",
                    "rh"   = "%",
                    "bend_angle" = "rad",
                    NULL)
  }
}

# getLatestDate
getLatestDate <- function(base,exp=NULL){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getLatestDate(",base,exp,")")) }

  date = NULL
  if ( !is.null(base) && !is.null(exp)){
    dtg=getLatestDTG(base,exp)
    if ( verbose("DEBUG")) { print(paste("DEBUG: Latest DTG=",dtg)) }
    
    dbConn <- connect(base,exp,dtg)
    if ( !is.null(dbConn)){
      query<-paste("SELECT dtg FROM obsmon ORDER BY dtg DESC LIMIT 1")
      if ( verbose("INFO")) { print(paste("INFO: ",query)) } 
      queryData <- data.frame(dbGetQuery(dbConn,query))
      disconnect(dbConn)
      date<-paste(substr(queryData$DTG,1,4),"-",substr(queryData$DTG,5,6),"-",substr(queryData$DTG,7,8),sep="")
    }else{
      if ( verbose("DEBUG")) { print(paste("Can not connect to ",base,exp,dtg)) }
    }
 
  }
  return(date)
}


# getLatestCycle
getLatestCycle <- function(base,exp=NULL){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getLatestCycle(",base,exp,")")) }

  cycle=NULL
  if ( !is.null(base) && !is.null(exp)){
    dtg=getLatestDTG(base,exp)
    if ( verbose("DEBUG")) { print(paste("DEBUG: Latest DTG=",dtg)) }

    if ( !is.null(dtg) ) {
      dbConn = connect(base,exp,dtg)
      if ( !is.null(dbConn)){
        query = paste("SELECT dtg FROM obsmon ORDER BY dtg DESC LIMIT 1")
        if (verbose("INFO")) { print(paste("INFO: ",query)) }  
        queryData = data.frame(dbGetQuery(dbConn,query))
        disconnect(dbConn)
        cycle = paste(substr(queryData$DTG,9,10),sep="")
      }else{
        if ( verbose("DEBUG")) { print(paste("Can not connect to ",base,exp,dtg)) }
      }
    }
  }
  return(cycle)
}

# getPastDate
getPastDate<-function(date,increment){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getPastDate(",date,increment,")")) }

  date = chron(dates=date,times=c(00:00:00),format=c('Y-m-d','h:m:s'))
  date=date-increment
  date=strftime(chron(date), "%Y-%m-%d")

  return(date)
}

# getLatestDTG
getLatestDTG<-function(base,exp){
  if ( verbose("DEBUG")) { print(paste("DEBUG: getLatestDTG(",base,exp,")")) }

  getLatestDTG=NULL
  if ( !is.null(base) && !is.null(exp) ){
    dir = getFile(base,exp,dir=T)
    if ( !is.null(dir)) {
      dtg=tail(dir(path=dir,pattern="[0-9]{10}"),1)
      if ( length(dtg) != 0 ) { getLatestDTG=dtg}
    }
  }
  return(getLatestDTG)
}

# getStations
getStations<-function(variable){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getStations(",variable,")")) }

  stations=NULL
  if ( !is.null(variable)) {
    base=NULL

    switch(variable,"U10M" = { base="Minimization"}, "V10M" = { base="Minimization"},"Z" = { base="Minimization"},{ base="Surface"})

    dtg2=getLatestDTG(base,input$experiment_SD)
    date2=dtg2date(dtg2)
    cycle=dtg2utc(dtg2)
    dtg2=date2dtg(date2,cycle)
    date1=getPastDate(date2,input$ndays)
    dtg1=date2dtg(date1,cycle)

    dbConn=connect(base,input$experiment_SD,dtg=dtg2)
    if ( !is.null(dbConn)){
      query<-paste("SELECT DISTINCT statid FROM usage WHERE DTG >=",dtg1," AND DTG <= ",dtg2," AND varname == '",tolower(variable),"' AND ( active == 1 OR anflag != 0 ) ORDER BY statid",sep="")
      if (verbose("INFO")) { print(paste("INFO: ",query)) }
      data <- data.frame(dbGetQuery(dbConn,query))
      stations=data$statid
      stations=gsub("'","",stations)
      stations=gsub(" ","",stations)
      name=getSynopName(stations)
      stations=paste(name,"  [",stations,"]",sep="")
      disconnect(dbConn)
    }
  }
  return(stations)
}


# verbose
verbose <- function(level){
  verb=FALSE
  if ( !is.null(level)) {
    # Default if level is misspelled is DEBUG
    switch(level,"DEBUG" = {verbositylevel=3}, "INFO" = {verbositylevel=2},"WARNING" = {verbositylevel=1}, "NONE" = {verbositylevel=0}, {verbositylevel=2})
    if ( !is.null(input$verbosity_chosen)) {
      if ( input$verbosity_chosen == "DEBUG" && verbositylevel <= 3) {
        verb=TRUE
      } else if ( input$verbosity_chosen == "INFO" && verbositylevel <= 2 ){
        verb=TRUE
      } else if ( input$verbosity_chosen == "WARNING" && verbositylevel <= 1 ){
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
getDumpData<-function(base,table,exp){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: getDumpData(",base,")"))}
  dumpData=NULL

  if ( !is.null(base) && !is.null(exp) && !is.null(table)){
    dbConn=connect(base,exp)
    if (!is.null(dbConn)){
      query<-paste("SELECT * from ",table,sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",query))}
      dumpData <- data.frame(dbGetQuery(dbConn,query))
      disconnect(dbConn)
    }
  }
  return(dumpData)
}

#
# SQLite and file functions
#

# getFile
getFile <- function(base,experiment,dtg=NULL,dir=F){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> getFile(",base,experiment,dtg,dir,")")) }
  fname=NULL
  if ( !is.null(base)) {
    # Set file name either from experiment description or from uploaded file
    if ( is.null(experiment)){ 
      if ( base == "Screening" ){
        if (!is.null(input$ODBbase_screening)){ fname <- input$ODBbase_screening$datapath }
      } else if ( base == "Minimization" ){
        if (!is.null(input$ODBbase_minimization)) { fname <- input$ODBbase_minimization$datapath }
      } else if ( base == "Surface" ){
        if (!is.null(input$ODBbase_surface)) { fname <- input$ODBbase_surface$datapath }
      }
    } else {
      fname<-setExperiment(experiment,base,dtg,dir)
    }
  }
  return(fname)
}


# connect
connect <- function(base,exp,dtg=NULL){
  if ( verbose("DEBUG")) { print(paste("DEBUG: -> connect(",base,exp,dtg,")")) }
  dbConn<-NULL
  if ( !is.null(base) && !is.null(exp)) {
    fname <- getFile(base,exp,dtg)
    if (!is.null(fname) && file.exists(fname) && file.info(fname)$size > 0 ){
      if ( verbose("DEBUG")) { print(paste("DEBUG: -> dbConnect ",fname)) }
      dbConn <- dbConnect(RSQLite::SQLite(),fname)
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

# Help functions for SQL extraction
# setLevelList
setLevelList<-function(selected_levels){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> setLevelList",selected_levels)) }

  if ( is.null(selected_levels) ){
    return("")
  }
  all=FALSE
  for ( i in 1:length(selected_levels)){
    if ( selected_levels[i] == "ALL" ) {
      all=TRUE
    }
    if ( selected_levels[i] == "Surface" ) {
      all=TRUE
    }
  }
  if ( all ) {
    return("")
  } else {
    if ( length(selected_levels) > 0 ) {
      sql=" AND ("
      for ( i in 1:length(selected_levels)){ 
        or=""
        if ( i != 1 ) { or=" OR " }
        sql=paste(sql,or,"( level == ",selected_levels[i],") ",sep="")
      }
      sql=paste(sql,")",sep="")
    }
    return(sql)
  }
}

# setChannelList
setChannelList<-function(selected_channels){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> setChannelList(",selected_channels,")")) }

  if ( is.null(selected_channels) ){
    return("")
  }
  if ( length(selected_channels) > 0 ) {
    sql=" AND ("
    for ( i in 1:length(selected_channels)){
      if ( selected_channels[i] != "ALL" ) {

        or=""
        if ( i != 1 ) { or=" OR " }
        sql=paste(sql,or,"( level == ",selected_channels[i],") ",sep="")
      }
    }
    sql=paste(sql,")",sep="")
  }
  return(sql)
}

# getSynopName
getSynopName<-function(number){
  #if ( verbose("DEBUG") ) { print(paste("DEBUG: -> getSynopName(",number,")")) }
 
  name=NA
  # Read synops first time
  if ( is.null(values$synops)) {
    if ( verbose("DEBUG") ) { print(paste("DEBUG: Reading file")) }
    stations=read.csv("allsynop.list.csv",sep=";",header=FALSE)
    values$synops = as.character(stations$V2)
    names(values$synops) = as.character(stations$V1)
  }
  
  name=values$synops[as.character(number)]
  return(name)
}

getDataTS<-function(base,exp,dtg1,dtg2,mode,plotQuery,utc=NULL){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> getDataTS(",base,exp,dtg1,dtg2,mode,plotQuery,")")) }

  getDataTS=data.frame()
  # Loop cycles and do queries
  if ( !is.null(base) && !is.null(exp) && !is.null(dtg1) && !is.null(dtg2) ){
    dir = getFile(base,exp,dir=T)
    y=dir(path=dir,pattern="[0-9]{10}")
    for (i in 1:length(y) ) {
      dtg=y[i]
      hh=substr(dtg,9,10)
      if ( is.null(utc) || hh == utc ){
        if ( dtg >= dtg1 && dtg <= dtg2 ) {
          if ( verbose("INFO") ) {print(paste("INFO: ",plotQuery))}
          if ( mode == "query" ) {return(plotQuery)}
          dbConn=connect(base,exp,dtg=dtg)
          if ( !is.null(dbConn)){
            getDataTS_tmp = data.frame(dbGetQuery(dbConn,plotQuery))
            if ( is.data.frame(getDataTS_tmp) && nrow(getDataTS_tmp)!=0 ){
              getDataTS=rbind(getDataTS,getDataTS_tmp)
            }else{
              if ( verbose("DEBUG") ) { print(paste("No data found for dtg=",dtg,sep=""))}
            }
            disconnect(dbConn)
          }
        }
      }
    }
  }
  return(getDataTS)
}

getRasterFromFile<-function(base,exp,dtg){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: ->getRasterFromFile(",base,exp,dtg,")"))}

  var="None"
  acc=""
  level=NULL
  if ( input$tabs == "Surface" ){
    if ( !is.null(input$accumulated_map_SA)){
      if ( input$accumulated_map_SA ) { acc="_acc"}
    }
    if ( !is.null(input$map_menu_SA)) { var=input$map_menu_SA}
  }else{
    if ( !is.null(input$accumulated_map)){
      if ( input$accumulated_map ) { acc="_acc"}
    }
    if ( !is.null(input$map_menu)) { var=input$map_menu}
    if ( !is.null(input$level_ncfile)) { level=input$level_ncfile}
  }

  r=NULL
  if ( var != "None" ){
    fname=getRasterFileName(base,exp,dtg,var,acc)
    if (!is.null(fname)){
      if (file.exists(fname)){
        if ( is.null(level)) {
          r = raster(fname,varname=var)
        }else{
          r = raster(fname,varname=var,level=as.numeric(level))
        }
      }
    }
  }
  return(r)
}

getRasterDir<-function(base,exp,dtg){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: ->getRasterDir(",base,exp,dtg,")"))}

  getRasterDir=NULL
  dir = getFile(base,exp,dir=T)
  if ( !is.null(dir)) {
    dtg_test=tail(dir(path=dir,pattern="[0-9]{10}"),1)
    if ( length(dtg_test) != 0 ) { 
      anacc_dir=paste(dir,"../anacc/",dtg,"/",sep="")
      #Old anacc_dir=paste(dir,"../anacc/",sep="")
      # Not working in R < 3.2 if (dir.exists(anacc_dir)){
      if (file_test("-d",anacc_dir)){
        getRasterDir=anacc_dir
      }
    }
  }
  return(getRasterDir)
}

getRasterFileName<-function(base,exp,dtg,var,acc){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: ->getRasterFileName(",base,exp,dtg,var,acc,")"))}

  fname=NULL
  dir=getRasterDir(base,exp,dtg)
  if (!is.null(dir)){
    if ( var != "None" ){
      if ( var == "TG1" || var == "TG2" || var == "WG1" || var == "WG2" || var == "WSNOW_VEG1" ){
        prefix="ansfc_sfx"
      }else if ( var == "air_temperature_ml" || var == "surface_air_pressure" || var == "air_temperature_ml" || var == "specific_humidity_ml" || var == "x_wind_ml" || var == "y_wind_ml") {
        prefix="anua"
      }else if ( var == "air_temperature_2m" || var == "relative_humidity_2m" || var == "liquid_water_content_of_surface_snow" ) {
        prefix="ansfc"
      }

      fnam=paste(dir,prefix,"_",dtg,acc,".nc4",sep="")
      #Old fnam=paste(dir,prefix,"_",texp,dtg,acc,".nc4",sep="")
      if (file.exists(fnam) && file.info(fnam)$size > 0 ){
        fname=fnam
      }
    }
  }
  return(fname)
}
