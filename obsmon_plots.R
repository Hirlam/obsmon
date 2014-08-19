
#
# General plotting interface controlling all of the plotting
# Organized in if tests for the different plot types
#

generatePlot<-function(odbBase,exp,plotName,obName,varName,levels,sensor,satelite,channels,dateRange,cycle) {
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generatePlot",odbBase,exp,plotName,obName,varName,levels,sensor,satelite,channels,dateRange,cycle)) }

  # Sanity checks on mandatory values!
  if ( is.null(odbBase) ) {
    if ( verbose("WARNING") ) { print("WARNING: The odb base for plotting is not defined!") }
    return(NULL)
  }
  if ( is.null(exp) ) {
    if ( verbose("WARNING") ) { print("WARNING: The experiment for plotting is not defined!") }
    return(NULL)
  }
  if ( is.null(plotName) ) {
    if ( verbose("WARNING") ) { print("WARNING: plotName you wanted to plot is not defined!") }
    return(NULL)
  }
  if ( is.null(obName) ) {
    if ( verbose("WARNING") ) { print("WARNING: The observation type for plotting is not defined!") }
    return(NULL)
  }
  if ( is.null(varName) ) {
    if ( verbose("WARNING") ) { print("WARNING: The variable name for plotting is not defined!") }
    return(NULL)
  }
  if ( is.null(dateRange) ) {
    if ( verbose("WARNING") ) { print("WARNING: The date range for plotting is not defined!") }
    return(NULL)
  }
  if ( is.null(cycle) ) {
    if ( verbose("WARNING") ) { print("WARNING: The cycle for plotting is not defined!") }
    return(NULL)
  }

  # Save these values to easier change values
  values$last_base=odbBase
  values$last_experiment=exp
  values$last_obtype=obName
  values$last_variable=varName
  values$last_level=c(levels)
  values$last_sensor=sensor
  values$last_satelite=satelite
  values$last_channel=c(channels)
 
  # Set needed values from mandatory
  obNumber=getObNumber(obName)
  if ( is.null(obNumber) ) {
    if ( verbose("WARNING") ) { print("WARNING: The obNumber for plotting is not defined!") }
    return(NULL)
  }
  unit=getUnit(varName)
  if ( is.null(unit) ) {
    if ( verbose("WARNING") ) { print("WARNING: The unit for plotting is not defined!") }
    return(NULL)
  }

  # Manipulate selected date
  date1 = dateRange[1]
  date2 = dateRange[2]
  # General settings
  dtg=date2dtg(date1,cycle)
  dtgbeg = date2dtg(date1,cycle)
  dtgend = date2dtg(date2,cycle)
  obname = tolower(obName)
  # DTG string for plotting
  dtgstr_range = paste("[",date1," ",paste0(cycle,"Z")," - ",date2," ",paste0(cycle,"Z]"),sep="")
  dtgstr = paste("[",date1," ",paste0(cycle,"Z"),"]",sep="")

  # Set level string
  if ( !is.null(levels)) {
    level_string=paste(" Level=(",paste(levels,sep="",collapse=","),") ",sep="")
    for ( i in 1:length(levels)){
      if ( levels[i] == "Surface" ){
        level_string=""
      }
      if ( levels[i] == "ALL" ){
        level_string=" Level=(ALL) "
      }
    }
  }else{
    level_string=""
  }
  # Set channel string
  if ( !is.null(channels)) {
    if ( length(channels) > 1 ){ 
      channel_string=paste(" Channels=(",paste(channels,sep="",collapse=","),")",sep="")
    }else{
      channel_string=paste(" Channel=",paste(channels,sep="",collapse="")," ",sep="")
    }
  }else{
    channel_string=""
  }


  obPlot=NULL
  dbConn<-connect(odbBase,exp)
  if ( !is.null(odbBase)){

# NumberOfObservations
    if ( plotName == "NumberOfObservations"){
      if (obNumber == "7") {
        qsatelite<-setDBSatname(as.character(satelite))
        channelListQuery<-setChannelList(channels)
        values$plotQuery<-paste("SELECT DTG,nobs_total,level FROM obsmon",
                           " WHERE obnumber = ",obNumber,
                           " AND DTG >= ",dtgbeg,
                           " AND DTG <= ",dtgend,
                           " AND obname == '",tolower(sensor),"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
        ylab="Channel"
        title=paste(plotName,obName,satelite,dtgstr_range)
      }else{
        levelListQuery<-setLevelList(levels)
        values$plotQuery<-paste("SELECT DTG,nobs_total,level FROM obsmon",
                           " WHERE obnumber = ",obNumber,
                           " AND DTG >= ",dtgbeg,
                           " AND DTG <= ",dtgend,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
        ylab="Level"
        title<-paste(plotName,obName,varName,level_string,dtgstr_range)
      }
      if ( verbose("INFO") ) { paste("INFO: ",print(values$plotQuery))}
      values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
      obPlot <- NumberOfObservations(title,ylab)

# FirstGuessDepartureMap
# FirstGuessBCDepartureMap
# AnalysisDepartureMap
# BiasCorrectionMap
# ObservationsMap
    } else if ( plotName == "FirstGuessDepartureMap" || plotName == "FirstGuessBCDepartureMap" || plotName == "AnalysisDepartureMap" || plotName == "BiasCorrectionMap" || plotName == "ObservationsMap" ) {
      if ( plotName == "FirstGuessDepartureMap" ){
        sql<-"fg_dep"
      }else if ( plotName == "FirstGuessBCDepartureMap" ){
        sql<-"fg_dep,biascrl"
      }else if ( plotName == "AnalysisDepartureMap" ){
        sql<-"an_dep"
      }else if ( plotName == "BiasCorrectionMap") {
        sql<-"biascrl"
      }else if ( plotName == "ObservationsMap") {
        sql<-"obsvalue"
      }

      if (obNumber == "7") {
        qsatelite<-setDBSatname(as.character(satelite))
        channelListQuery<-setChannelList(channels)
        values$plotQuery<-paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",tolower(sensor),"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
        title<-paste(plotName,obName,satelite,channel_string,dtgstr)
      }else{
        levelListQuery<-setLevelList(levels)
        values$plotQuery<-paste("SELECT latitude,longitude,statid,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
        title<-paste(plotName,obName,varName,level_string,dtgstr)
      }
      if ( verbose("INFO") ) { paste("INFO: ",print(values$plotQuery))}
      values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
      if ( plotName == "FirstGuessDepartureMap" ){
        values$plotData$values<-values$plotData$fg_dep
      }else if ( plotName == "FirstGuessBCDepartureMap" ){
        values$plotData$values<-values$plotData$fg_dep+values$plotData$biascrl
      }else if ( plotName == "AnalysisDepartureMap" ){
        values$plotData$values<-values$plotData$an_dep
      }else if ( plotName == "BiasCorrectionMap") {
        values$plotData$values<-values$plotData$biascrl
      }else if ( plotName == "ObservationsMap") {
        values$plotData$values<-values$plotData$obsvalue
      }
      if ( nrow(values$plotData) > 0 ) {
        obPlot <- ThresholdMap(title)
      }else{
        obPlot<-emptyPlot(title)
      }
      
# ObservationUsage
    }else if ( plotName == "ObservationUsage" ) {
      # SATEM
      if (obNumber == "7") {

        qsatelite<-setDBSatname(as.character(satelite))
        channelListQuery<-setChannelList(channels)
        values$plotQuery<-paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND DTG == ",dtg,
                           " AND obname == '",tolower(sensor),"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
        title<-paste(plotName,sensor,satelite,channel_string,dtgstr)
        values$plotData <- dbGetQuery(dbConn,values$plotQuery)
        if ( nrow(values$plotData) > 0 ) {
          obPlot<-usageMap(title)
        }else{
          obPlot<-emptyPlot(title)
        }
      } else {
        # Non SATEMs
        levelListQuery<-setLevelList(levels)
        obPlot<-NULL
        values$plotQuery<-paste("SELECT latitude,longitude,statid,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
        title<-paste(plotName,obName,varName,level_string,dtgstr)
        values$plotData <- dbGetQuery(dbConn,values$plotQuery)
        if ( nrow(values$plotData) > 0 ) {
          obPlot<-usageMap(title)
        }else{
          obPlot<-emptyPlot(title)
        }
      }
# Bias correction time-series
    } else if ( plotName == "BiasCorrection" || plotName == "Hovmoller" ) {
      # SATEM
      if (obNumber == "7") {
        qsatelite<-setDBSatname(as.character(satelite))
        channelListQuery<-setChannelList(channels)
        values$plotQuery <- paste("SELECT DTG,fg_bias_total,fg_uncorr_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND obname == '",tolower(sensor),"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG >= ",dtgbeg,
                             " AND DTG <= ",dtgend,
                             channelListQuery,sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
        title<-paste(plotName,satelite,sensor,channel_string,dtgstr_range,sep=" ")
        values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
        if ( nrow(values$plotData) > 0 ) {
          obPlot <- SatBcorrCycle(title,cycle,plotName)
        }else{
          obPlot<-emptyPlot(title)
        }
      # Non-SATEMSs do not have bias correction from ODB
      } else {
         obPlot=NULL
      }
# FGAnDeparture
    } else if ( plotName == "FGAnDeparture" ) {
      # SATEM
      if (obNumber == "7") {
        qsatelite<-setDBSatname(as.character(satelite))
        values$plotQuery <- paste("SELECT DTG,fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber ==",obNumber,
                             " AND obname == '",tolower(sensor),"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG == ",dtg,
                             " AND level>=1 AND level<=10000",
                             " ORDER BY level",sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
        title<-paste(plotName,sensor,satelite,dtgstr)
        values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
        if ( nrow(values$plotData) > 0 ) {
          ylab<-"Channels"
          xlab<-"Brightness temperature [K]"
          obPlot<-FGAnDepartureVert(title,xlab,ylab)
        }else{
          obPlot<-emptyPlot(title)
        }
      # Non-SATEMs
      }else{
        # Surface
        if ( obNumber == 1 || obNumber == 4 ) {
          if ( verbose("DEBUG") ) { print(paste("DEBUG: ",obname))}
          values$plotQuery<-paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND dtg == ",dtg,
                             " AND obname == '",obname,"'",
                             " AND varname == '",varName,"'",sep="")
          if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery)) }
          title<-paste(plotName,obName,varName,dtgstr)
          values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
          if ( nrow(values$plotData) > 0 ) {
            title<-paste(plotName,obName,varName,dtgstr)
            xlab<-paste(varName)
            ylab<-paste("Bias/RMS (",unit,")")
            obPlot<-FGAnDeparture(title,xlab,ylab)
          }else{
            obPlot<-emptyPlot(title)
          }
        } else {
          # Vertical profile (Aircraft/Temp)
          values$plotQuery<-paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,level,varname FROM obsmon ",
                             " WHERE obnumber == ",obNumber,
                             " AND dtg == ",dtg,
                             " AND obname == '",obname,"'",
                             " AND varname == '",varName,"'",
                             " ORDER BY level",sep="")
          if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
          title<-paste(plotName,obName,varName,dtgstr)
          values$plotData <- data.frame(dbGetQuery(dbConn,values$plotQuery))
          if ( nrow(values$plotData) > 0 ) {
            xlab<-paste("(",unit,")")
            ylab<-"Pressure"
            obPlot<-FGAnDepartureVert(title,xlab,ylab)
          }else{
            obPlot<-emptyPlot(title)
          }
        }
      }
    }else{
      if ( verbose("WARNING")) { print(paste("WARNING: ",plotName,"not found!")) }
      obPlot<-emptyPlot(paste(plotName,"not found!"))
    }
    disconnect(dbConn)
  }
  return(obPlot)
}

##################################################################################
#                                                                                #
#                 PLOTTING OF DIFFERENT PLOTS                                    #
#                                                                                #
##################################################################################

  # Empty plot
  ggimage <- function(image){
    if(length(dim(image)) == 2){
      message('creating black and white image...')
      image <- melt(image)
      names(image) <- c('row','column','fill')
      plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
        scale_fill_gradient(low = 'black', high = 'white')
    }
  
    if(length(dim(image)) == 3){
  	  message('creating color image...')
  	  image <- apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
      image <- melt(image)
      names(image) <- c('row', 'column', 'fill')
      plot <- qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()  	
    }
  }
  # Empty plot function
  emptyPlot<-function(title){
    image <- readJPEG("./nodata.jpg")
    obPlot<-ggimage(image) + coord_equal() + labs(title=title)
    return(obPlot)
  }

  # ThresholdMap
  ThresholdMap<-function(title){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> ThresholdMap",title)) }

    obPlot<-NULL

    x1=min(values$plotData$longitude)-2
    x2=max(values$plotData$longitude)+2
    y1=min(values$plotData$latitude)-2
    y2=max(values$plotData$latitude)+2

    obPlot<-ggplot(map.world,aes(long,lat))
    obPlot<-obPlot + geom_path(data=map.world, aes (group = group),colour="black",show_guide=FALSE) +
                     coord_map("stereographic",xlim=c(x1,x2),ylim=c(y1,y2)) +
                     geom_point(data=values$plotData,aes(x=longitude,y=latitude,colour=values),size=3) +
                     scale_colour_gradientn(colours = rainbow(15)) +
                     ylab("lat") +
                     xlab("lon") +
                     labs(title = title)
    return(obPlot)
  }

  # FGAnDeparture
  FGAnDeparture <- function(title,xlab,ylab){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDeparture",title,xlab,ylab)) }

    df <- data.frame(params = factor(c("FGBias", "AnBias", "FGRMS",  "AnRMS"), c("FGBias", "AnBias", "FGRMS",  "AnRMS")), biasRMSvalues = c(values$plotData$fg_bias_total, values$plotData$an_bias_total, values$plotData$fg_rms_total, values$plotData$an_rms_total))
    obPlot <- ggplot(data=df, aes(x=params, y=biasRMSvalues, fill=c("red","darkblue","red","darkblue"))) + geom_bar(stat="identity") + guides(fill=FALSE) + ylab(ylab) + xlab(xlab)

    obPlot <- obPlot + labs(title=title)
    return(obPlot)
  }

  # FGAnDepartureVert
  FGAnDepartureVert<- function(title,xlab,ylab){
     if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDepartureVert",title,xlab,ylab)) }
     obPlot <- NULL
     lastLevel<-values$plotData$level[length(values$plotData$level)]
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",values$plotData$level))}
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",lastLevel))}
     if ( lastLevel > 90000 ) {
       obPlot <- ggplot(values$plotData, aes(level)) +
               geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
               geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
               geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
               geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
               geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
               geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
               geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
               geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
               coord_flip()+
               scale_x_continuous(breaks=values$plotData$level) +
               # Rotated beacuse of coord_flip
               ylab(xlab)+
               xlab(ylab) +
               xlim(100000,0) +
               labs(title = title)
     }else{
       obPlot <- ggplot(values$plotData, aes(level)) +
               geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
               geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
               geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
               geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
               geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
               geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
               geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
               geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
               coord_flip()+
               scale_x_continuous(breaks=values$plotData$level) +
               # Rotated beacuse of coord_flip
               ylab(xlab) +
               xlab(ylab) +
               labs(title = title)
     }
     return(obPlot)
  }

  # usageMap
  usageMap<- function(title){
    status<-rep("NA",length(values$plotData$longitude))
    status<- ifelse(values$plotData$anflag == 0,"Rejected",status)
    status<- ifelse(values$plotData$active > 0,"Active",status)
    status<- ifelse(values$plotData$anflag > 0,"Active",status)


    values$plotData$status=status

    x1=min(values$plotData$longitude)-2
    x2=max(values$plotData$longitude)+2
    y1=min(values$plotData$latitude)-2
    y2=max(values$plotData$latitude)+2
 
    obPlot<-ggplot(map.world,aes(long,lat))
    obPlot<-obPlot + geom_path(data=map.world, aes (group = group), colour="black") +
                     coord_map("stereographic",xlim=c(x1,x2),ylim=c(y1,y2)) +
                     geom_point(data=values$plotData[rev(order(status)),],aes(x=longitude,y=latitude,colour=status),size=3) +
                     ylab("lat") +
                     xlab("lon") +
                     labs(title = title) +
                     scale_colour_manual(name="Legend",values=c("Active"="green","Active(2)"="blue","Rejected"="red","Passive"="yellow","Blacklisted"="black","NA"="grey"))
    return(obPlot)
  }

  # SatBcorrCycle
  SatBcorrCycle <- function(title,cycle,plotName){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> SatBcorrCycle",title,cycle,plotName)) }

    if ( plotName == "Hovmoller" ){
      localPlotData=values$plotData
      localPlotData$datetime = chron(dates=dtg2date(values$plotData$DTG),times=paste(dtg2utc(values$plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))

      obPlot = ggplot(localPlotData,aes(x=datetime,y=level,fill=fg_bias_total))+geom_raster()
      obPlot = obPlot + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
      obPlot = obPlot + xlab("DATE")+ylab("Channels")
      obPlot = obPlot + labs(title=paste(title," (All cycles)"))
    }else if ( plotName == "BiasCorrection" ){
      LXX = as.integer(dtg2utc(values$plotData$DTG))==as.integer(cycle)
      print(LXX)
      plotDataXX <- values$plotData[LXX,]
      localPlotDataXX=plotDataXX
      localPlotDataXX$datetime = chron(dates=dtg2date(plotDataXX$DTG),times=paste(dtg2utc(plotDataXX$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))
 
      obPlot <- ggplot(data=localPlotDataXX)
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_uncorr_total,colour="FGdep raw"))
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_bias_total,colour="FGdep bcorr"))
      obPlot <- obPlot + xlab("DATE")+ylab("T [K]")
      obPlot <- obPlot + labs(title=paste(title,cycle," UTC"))
      obPlot <- obPlot + geom_text(aes(x=DTG,y=fg_uncorr_total,label=nobs_total))
      obPlot <- obPlot + facet_wrap(~ level)
    }
    return(obPlot)
  }

  # NumberOfObservations
  NumberOfObservations<- function(title,ylab){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> NumberOfObservations",title)) }

    localPlotData=values$plotData
    localPlotData$datetime = chron(dates=dtg2date(values$plotData$DTG),times=paste(dtg2utc(values$plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))
    
    obPlot<-ggplot(localPlotData,aes(x=datetime,y=level,fill=nobs_total))+geom_raster()
    obPlot <- obPlot + xlab("DATE") + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
    obPlot <- obPlot + labs(title=title,ylab=ylab)

    return(obPlot) 
  }




generate_surfdia <- function(var,station,exp){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generate_surfdia",var,station,exp)) }

  if ( !is.null(var) && !is.null(station)){

    base=NULL
    switch(var,"U10M" = { base="Minimization"}, "V10M" = { base="Minimization"},"Z" = { base="Minimization"},{ base="Surface"})

    values$last_variable=var
    values$last_station=station

    date2=getLatestDate(base,exp)
    date1=getPastDate(date2,input$ndays)
    cycle=getLatestCycle(base,exp)
    dtg2=date2dtg(date2,cycle)
    dtg1=date2dtg(date1,cycle)

    stationstr=strsplit(station,'\\[')
    station2=gsub('\\]','',stationstr[[1]][2])

    obPlot=NULL
    plotQuery<-paste("SELECT dtg,obsvalue,fg_dep,an_dep,statid FROM usage ",
                             " WHERE statid LIKE '%",station2,"%'",
                             " AND DTG >= ",dtg1," AND DTG <= ",dtg2,
                             " AND obname == 'synop' ",
                             " AND varname == '",tolower(var),"'",sep="")
    if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
    title<-paste(exp,var,station)
    dbConn=connect(base,exp)
    if ( !is.null(dbConn)){
      plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
      if ( nrow(plotData) > 0 ) {
        plotData$datetime = chron(dates=dtg2date(plotData$DTG),times=paste(dtg2utc(plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))

        obPlot <- ggplot(plotData,aes(x=datetime,y=obsvalue),group="")
        obPlot <- obPlot + geom_line(aes(y=obsvalue,colour="Obs",group=""))
        obPlot <- obPlot + geom_line(aes(y=obsvalue-fg_dep,colour="obs - FG dep",group=""))
        obPlot <- obPlot + geom_line(aes(y=obsvalue-an_dep,colour="obs - AN dep",group=""))
        obPlot <- obPlot + xlab("DATE") + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
        obPlot <- obPlot + scale_colour_manual(values=c("black", "green","red"))
        obPlot <- obPlot + labs(title=title,ylab=ylab)
      }
      disconnect(dbConn)
    }
    return(obPlot)
  }else{
    return(NULL)
  }
}
 
