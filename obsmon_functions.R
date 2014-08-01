#
# Help functions and settings for shiny interface
#

default_obtypes     <- c("SYNOP","SHIP","AIRCRAFT","DRIBU","TEMP","SATEM","RADAR")
listOfSensors       <- c("AMSUA","AMSUB","MHS","IASI")
verbosity           <- 10

# Normal plots
plotTypesStat       <- c("FG+An departure")
plotTypesMaps       <- c("Observation usage (map)","First guess departure (map)","Analysis departure (map)","Observations (map)")
plotTypesTS         <- c("Bias correction (TS)","Hovmoeller (TS)")
plotTypesSat        <- c("FG dep + Bias correction (map)","Bias correction (map)")

default_experiments <- c("MetCoOp","Shiny environment","DMI")
#default_experiments <- c("MetCoOp","DMI")

# Predefined plots
preDefinedGroups    <- c("MetCoOp","DMI")
preDefinedPlots     <- c("Plot 1","Plot 2")

# setExperiment
setExperiment <- function(exp,base){
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

    print(exp)
    print(dbtry_ecma)
    print(dbtry_ecma_sfc)
    print(dbtry_ccma)

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
  }
  return (setExperiment)
}

# getExperiments
getExperiments <- function(base){
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

# getObtypes
getObtypes <- function(base){
  if ( !is.null(base)) {
    if ( base == "Surface" ){
      return(c("SYNOP"))
    }else{
      return (default_obtypes)
    }
  }
}

# getObNumber
getObNumber <- function(obtype){
  if ( !is.null(obtype)) {
    switch(obtype,"SYNOP"    = c("1"),
                  "SHIP"     = c("1"),
                  "AIRCRAFT" = c("2"),
                  "DRIBU"    = c("4"),
                  "TEMP"     = c("5"),
                  "SATEM"    = c("7"),
                  "RADAR"    = c("13"),
                  "Undefined"
    )
  }
}

# setQSatname
setQSatname<-function(sat){
  if ( !is.null(sat)) {
    switch(sat,
           "NOAA-15" = satname<-"noaa15",
           "NOAA-16" = satname<-"noaa16",
           "NOAA-17" = satname<-"noaa17",
           "NOAA-18" = satname<-"noaa18",
           "NOAA-19" = satname<-"noaa19",
           "METOP-A" = satname<-"metop2",
           "METOP-B" = satname<-"metop1",
           "Undefined"
    )
  }
}

# getPlotTypes
getPlotTypes <- function(obtype,dateRange){
  if ( !is.null(obtype)) {
    switch(obtype,"SATEM" = c(plotTypesStat,plotTypesTS,plotTypesMaps,plotTypesSat),c(plotTypesStat,plotTypesMaps))
  }
}

# getPlotTypeShort
getPlotTypeShort <- function(plotType){
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
           "Undefined")
  }
}

# getPreDefinedPlots
getPreDefinedGroups <- function(){
  return(preDefinedGroups)
}

# getPreDefinedPlots
getPreDefinedPlots <- function(group){
  if (!is.null(group)){
    switch(group,
               "MetCoOp" = c("Plot 1"," Plot 2"),
               "No plots defined!"
   )
  }
}


# getVariables
getVariables <- function(obtype,base){
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

    switch(obtype, "SYNOP"    = c(synop_vars),
                   "SHIP"     = c(ship_vars),
                   "AIRCRAFT" = c(aircraft_vars),
                   "DRIBU"    = c(dribu_vars),
                   "TEMP"     = c(temp_vars),
                   "RADAR"    = c(radar_vars_z,radar_vars_p),
                   "Undefined")
  }
}

# getLevels
getLevels <- function(obtype,var,plotType){
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

# getSensors
getSensors <- function(){
  return(listOfSensors)
}

# getSatelites
getSatelites <- function(sensor){

  if ( !is.null(sensor)) {
    # Set satelites pr. sensor
    switch(sensor, "AMSUA" = c("NOAA-15","NOAA-16","NOAA-17","NOAA-18","NOAA-19","METOP-A","METOP-B"),
                   "AMSUB" = c("NOAA-15","NOAA-16","NOAA-17","NOAA-18"),
                   "MHS"   = c("NOAA-19","METOP-A","METOP-B"),
                   "IASI"  = c("METOP-A","METOP-B"),
                   "Undefined")
  }
}

# getChannels
getChannels <- function(sensor){
  if ( !is.null(sensor)) {
    listOfChannelsAMSUA <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
    listOfChannelsAMSUB <- c("1","2","3","4","5")
    listOfChannelsMHS   <- c("1","2","3","4","5")
    listOfChannelsIASI  <- c("16","38","49","51","55","57","59","61","63","66","70","72","74","79","81","83","85","87","89","92","95","97","99","101","104","106","109","111","113","116","119","122","125","128","131","133","135","138","141","144","146","148","151","154","157","159","161","163","165","167","170","173","176","178","179","180","183","185","187","189","191","193","195","197","199","201","203","205","207","210","212","214","217","219","222","224","226","228","230","232","234","236","239","241","242","243","246","249","252","254","256","258","260","262","265","267","269","271","272","273","275","278","280","282","284","286","288","290","292","294","296","299","301","303","306","308","310","312","314","316","318","320","323","325","327","329","331","333","335","337","339","341","343","345","347","350","352","354","356","358","360","362","364","366","369","371","373","375","377","379","381","383","386","389","398","401","404","407","410","414","416","426","428","432","434","439","445","457","515","546","552","559","566","571","573","646","662","668","756","867","906","921","1027","1046","1090","1121","1133","1191","1194","1271","1479","1509","1513","1521","1536","1574","1578","1579","1585","1587","1626","1639","1643","1652","1658","1671","1786","1805","1884","1946","1991","2019","2094","2119","2213","2239","2245","2271","2321","2398","2701","2741","2745","2819","2889","2907","2910","2919","2939","2944","2948","2951","2958","2977","2985","2988","2991","2993","3002","3008","3014","3027","3029","3036","3047","3049","3053","3058","3064","3069","3087","3093","3098","3105","3107","3110","3127","3136","3151","3160","3165","3168","3175","3178","3207","3228","3244","3248","3252","3256","3263","3281","3303","3309","3312","3322","3339","3375","3378","3411","3438","3440","3442","3444","3446","3448","3450","3452","3454","3458","3467","3476","3484","3491","3497","3499","3504","3506","3509","3518","3522","3527","3540","3555","3575","3577","3580","3582","3586","3589","3599","3645","3653","3658","3661","3943","4032","5130","5368","5371","5379","5381","5383","5397","5399","5401","5403","5405","5455","5480","5483","5485","5492","5502","5507","5509","5517","5558","5988","5992","5994","6003","6350","6458","6463","6601","6962","6978","6980","6982","6985","6987","6989","6991","6993","6995","6997","7001","7267","7269","7389","7424","7426","7428","7885","8007")

    # Set channels
    switch(sensor, "AMSUA" = c("ALL",listOfChannelsAMSUA),
                   "AMSUB" = c("ALL",listOfChannelsAMSUB),
                   "MHS"   = c("ALL",listOfChannelsMHS),
                   "IASI"  = c("ALL",listOfChannelsIASI),
                   "Undefined")
  }
}

getUnit<-function(varName){
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

#
# SQLite and file functions
#

# getFile
getFile <- function(base){
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
}


# connect
connect <- function(base){
  dbConn<-NULL
  if ( !is.null(base)) {
    print(input$experiment)
    print(base)
    fname <- getFile(base)
    print(fname)
    if (!is.null(fname) && file.exists(fname)){
      dbConn <- dbConnect("SQLite",fname)
    }
  }
  return(dbConn)
}

# disconnect
disconnect <- function(dbConn){
  if ( !is.null(dbConn)) {
    dbDisconnect(dbConn)
  }
}

