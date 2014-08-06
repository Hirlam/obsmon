

  generatePlot<-function(odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle) {
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generatePlot",odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle)) }
 
    date1<-dateRange[1]
    date2<-dateRange[2]
    # General settings
    dtg<-gsub("-","",paste0(date1,cycle))
    dtgbeg <- paste0(gsub("-","",date1),cycle)
    dtgend <- paste0(gsub("-","",date2),cycle)
    obname <- tolower(obName)
    dtgstr <- paste(date1, paste0(cycle,"Z"))

    if ( is.na(obNumber)) {
      obPlot<-NULL
      return(obPlot)
    }else{

      dbConn<-connect(odbBase)
      obPlot<-NULL
      if ( plotName == "FirstGuessDepartureMap" || plotName == "FirstGuessBCDepartureMap" || plotName == "AnalysisDepartureMap" || plotName == "BiasCorrectionMap" || plotName == "ObservationsMap" ) {
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
          qsatelite<-setQSatname(as.character(satelite))
          channelListQuery<-setChannelList(channel,obname,satelite)
          values$plotQuery<-paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
          title<-paste(plotName,obName,satelite," channel=",channel,dtgstr)
        }else{
          levelListQuery<-setLevelList(channel)

          values$plotQuery<-paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
          title<-paste(plotName,obName,varName," Level=",channel,dtgstr)
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
      
#####################
#-------------------#
# Satellites        #
#-------------------#
#####################

      }else if (obNumber == "7") {
        qsatelite<-setQSatname(as.character(satelite))

#
# Usage map, satellite
#
        if ( plotName == "ObservationUsage" ) {
          channelListQuery<-setChannelList(channel,obname,satelite)
          values$plotQuery<-paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND DTG == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
          if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
          title<-paste(plotName,sensor,satelite," channel=",channel,dtgstr)
          values$plotData <- dbGetQuery(dbConn,values$plotQuery)
          if ( nrow(values$plotData) > 0 ) {
            obPlot<-usageMap(title)
          }else{
            obPlot<-emptyPlot(title)
          }
#
# Bias correction time-series
#
        } else if ( plotName == "BiasCorrection" || plotName == "Hovmoller" ) {
          channelListQuery<-setChannelList(channel,obname,satelite)
          values$plotQuery <- paste("SELECT DTG,fg_bias_total,fg_uncorr_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND obname == '",obname,"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG >= ",dtgbeg,
                             " AND DTG <= ",dtgend,
                             channelListQuery,sep="")
          if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
          values$plotData <- data.frame(DTG=character(),
                     "fg_bias_total"=numeric(),
                   "fg_uncorr_total"=numeric(),
                        "nobs_total"=numeric())
          title<-paste(plotName,satelite,sensor," Channel=",channel,sep=" ")
          values$plotData <- dbGetQuery(dbConn,values$plotQuery)
          if ( nrow(values$plotData) > 0 ) {
            values$plotData$DTG <- paste0(substr(values$plotData$DTG,1,4),"-",
                                         substr(values$plotData$DTG,5,6),"-",
                                         substr(values$plotData$DTG,7,8)," ",
                                         substr(values$plotData$DTG,9,10),":00")
            values$plotData$DTG <- as.POSIXct(values$plotData$DTG,"%Y-%m-%d %h:%m")
            obPlot <- SatBcorrCycle(title,cycle,plotName)
          }else{
            obPlot<-emptyPlot(title)
          }
#
# FGAnDeparture
#             
        } else if ( plotName == "FGAnDeparture" ) {
          values$plotQuery <- paste("SELECT DTG,fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber ==",obNumber,
                             " AND obname == '",obname,"'",
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
        }else{
          print(paste(plotName,"not found!"))
          obPlot<-emptyPlot(paste(plotName,"not found!"))
        }
#####################
#-------------------#
# Conventional      #
#-------------------#
#####################
      } else {
 
#
# Usage maps
#
        if ( plotName == "ObservationUsage" ) {
          # Surface
          levelListQuery<-setLevelList(channel)
          obPlot<-NULL
          values$plotQuery<-paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
          if ( verbose("INFO") ) { print(paste("INFO: ",values$plotQuery))}
          title<-paste(plotName,obName,varName," Level=",channel,dtgstr)
          values$plotData <- dbGetQuery(dbConn,values$plotQuery)
          if ( nrow(values$plotData) > 0 ) {
            obPlot<-usageMap(title)
          }else{
            obPlot<-emptyPlot(title)
          }

#
# Bias and RMS 
#
        } else if ( plotName == "FGAnDeparture" ) {
          # Surface
  	      if ( obNumber == 1 || obNumber == 4 ) {
            if ( verbose("DEBUG") ) { print(paste("DEBUG: ",obname))}
            obPlot<-NULL
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
            obPlot<-NULL
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
        }else{
          if ( verbose("INFO") ) { print(paste("INFO: ",plotName,"not found!")) }
          obPlot<-emptyPlot(paste(plotName,"not found!"))
        }
      }
      disconnect(dbConn)
      return(obPlot)
    }
  }

setLevelList<-function(level){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> setLevelList",level)) }

  if ( is.null(level) ){ level="ALL" }
  sql<-""
  levelList<-input$level
  lev<-as.character(level)
  test<-grep("-",lev)
  if ( lev == "ALL" || lev == "Surface" ){
    # Empty string, return sql
  } else if (length(test)>0){
    levels<-unlist(strsplit(lev, "-"))
    sql<-paste(sql,"AND (")
    first<-TRUE
    for (i in 1:length(levelList)){
      if ( as.numeric(levelList[i]) >= as.numeric(levels[1]) && as.numeric(levelList[i]) <= as.numeric(levels[2]) ){
        if ( ! first ) {sql<-paste(sql," OR ")}
        sql<-paste(sql," level == ",levelList[i],sep="")
        first<-FALSE
      }
    }
    sql<-paste(sql,")")
  }else{
    sql<-paste(sql,"AND ( level == ",level,")")
  }
  return(sql)
}

###########
#
# PLOTTING
#
###########

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
  FGAnDeparture <- function(title,xlab,ylab){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDeparture",title,xlab,ylab)) }

    df <- data.frame(params = factor(c("FGBias", "AnBias", "FGRMS",  "AnRMS"), c("FGBias", "AnBias", "FGRMS",  "AnRMS")), biasRMSvalues = c(values$plotData$fg_bias_total, values$plotData$an_bias_total, values$plotData$fg_rms_total, values$plotData$an_rms_total))
    obPlot <- ggplot(data=df, aes(x=params, y=biasRMSvalues, fill=c("red","darkblue","red","darkblue"))) + geom_bar(stat="identity") + guides(fill=FALSE) + ylab(ylab) + xlab(xlab)

    obPlot <- obPlot + labs(title=title)
    return(obPlot)
  }
  FGAnDepartureVert<- function(title,xlab,ylab){
     if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDepartureVert",title,xlab,ylab)) }
     obPlot <- NULL
     lastLevel<-values$plotData$level[length(values$plotData$level)]
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",values$plotData$level))}
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",lastLevel))}
     if ( lastLevel > 9000 ) {
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
  SatBcorrCycle <- function(title,cycle,plotName){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> SatBcorrCycle",title,cycle,plotName)) }

    obPlot<-NULL
    if ( plotName == "Hovmoller" ){
      obPlot<-ggplot(values$plotData,aes(x=DTG,y=level,fill=fg_bias_total))+geom_raster()
      obPlot <- obPlot + xlab("DATE")+ylab("Channels")
      obPlot <- obPlot + labs(title=paste(title," (All cycles)"))
    }else if ( plotName == "BiasCorrection" ){
      LXX = hour(values$plotData$DTG)==as.integer(cycle)
      values$plotDataXX <- values$plotData[LXX,]
      obPlot <- ggplot(data=values$plotDataXX)
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_uncorr_total,colour="FGdep raw"))
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_bias_total,colour="FGdep bcorr"))
      obPlot <- obPlot + xlab("DATE")+ylab("T [K]")
      obPlot <- obPlot + labs(title=paste(title,cycle," UTC"))
      obPlot <- obPlot + geom_text(aes(x=DTG,y=fg_uncorr_total,label=nobs_total))
      obPlot <- obPlot + facet_wrap(~ level)
    }
    return(obPlot)
  }

