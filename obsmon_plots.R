

  generatePlot<-function(odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle) {
   
    print(odbBase)
    date1<-dateRange[1]
    print(date1)
    date2<-dateRange[2]
    print(date2)
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
          plotQuery<-paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
          title<-paste(plotName,obName,satelite," channel=",channel,dtgstr)
        }else{
          levelListQuery<-setLevelList(channel)

          plotQuery<-paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
          title<-paste(plotName,obName,varName," Level=",channel,dtgstr)
        }
        if ( verbosity > 0 ) {print(plotQuery)}
        plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
        if ( plotName == "FirstGuessDepartureMap" ){
          plotData$values<-plotData$fg_dep
        }else if ( plotName == "FirstGuessBCDepartureMap" ){
          plotData$values<-plotData$fg_dep+plotData$biascrl
        }else if ( plotName == "AnalysisDepartureMap" ){
          plotData$values<-plotData$an_dep
        }else if ( plotName == "BiasCorrectionMap") {
          plotData$values<-plotData$biascrl
        }else if ( plotName == "ObservationsMap") {
          plotData$values<-plotData$obsvalue
        }
        if ( nrow(plotData) > 0 ) {
          obPlot <- ThresholdMap(title,plotData)
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
          plotQuery<-paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND DTG == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
          if ( verbosity > 0 ) {print(plotQuery)}
          title<-paste(plotName,sensor,satelite," channel=",channel,dtgstr)
          plotData <- dbGetQuery(dbConn,plotQuery)
          if ( nrow(plotData) > 0 ) {
            obPlot<-usageMap(title,plotData)
          }else{
            obPlot<-emptyPlot(title)
          }
#
# Bias correction time-series
#
        } else if ( plotName == "BiasCorrection" || plotName == "Hovmoller" ) {
          channelListQuery<-setChannelList(channel,obname,satelite)
          plotQuery <- paste("SELECT DTG,fg_bias_total,fg_uncorr_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND obname == '",obname,"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG >= ",dtgbeg,
                             " AND DTG <= ",dtgend,
                             channelListQuery,sep="")
          if ( verbosity > 0 ) {print(plotQuery)}
          plotData <- data.frame(DTG=character(),
                     "fg_bias_total"=numeric(),
                   "fg_uncorr_total"=numeric(),
                        "nobs_total"=numeric())
          title<-paste(plotName,satelite,sensor," Channel=",channel,sep=" ")
          plotData <- dbGetQuery(dbConn,plotQuery)
          if ( nrow(plotData) > 0 ) {
            plotData$DTG <- paste0(substr(plotData$DTG,1,4),"-",
                                         substr(plotData$DTG,5,6),"-",
                                         substr(plotData$DTG,7,8)," ",
                                         substr(plotData$DTG,9,10),":00")
            plotData$DTG <- as.POSIXct(plotData$DTG,"%Y-%m-%d %h:%m")
            obPlot <- SatBcorrCycle(plotData,title,cycle,plotName)
          }else{
            obPlot<-emptyPlot(title)
          }
#
# FGAnDeparture
#             
        } else if ( plotName == "FGAnDeparture" ) {
          plotQuery <- paste("SELECT DTG,fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber ==",obNumber,
                             " AND obname == '",obname,"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG == ",dtg,
                             " AND level>=1 AND level<=10000",
                             " ORDER BY level",sep="")
          if ( verbosity > 0 ) {print(plotQuery)}
          title<-paste(plotName,sensor,satelite,dtgstr)
          plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
          if ( nrow(plotData) > 0 ) {
            ylab<-"Channels"
            xlab<-"Brightness temperature [K]"
            obPlot<-FGAnDepartureVert(title,xlab,ylab,plotData)
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
          plotQuery<-paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           levelListQuery,sep="")
          if ( verbosity > 0 ) {print(plotQuery)}
          title<-paste(plotName,obName,varName," Level=",channel,dtgstr)
          plotData <- dbGetQuery(dbConn,plotQuery)
          if ( nrow(plotData) > 0 ) {
            obPlot<-usageMap(title,plotData)
          }else{
            obPlot<-emptyPlot(title)
          }

#
# Bias and RMS 
#
        } else if ( plotName == "FGAnDeparture" ) {
          # Surface
  	      if ( obNumber == 1 || obNumber == 4 ) {
            if ( verbosity > 2 ) {print(obname)}
            obPlot<-NULL
            plotQuery<-paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND dtg == ",dtg,
                             " AND obname == '",obname,"'",
	                         " AND varname == '",varName,"'",sep="")
            if ( verbosity > 0 ) {print(plotQuery)}
            title<-paste(plotName,obName,varName,dtgstr)
            plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
            if ( nrow(plotData) > 0 ) {
              title<-paste(plotName,obName,varName,dtgstr)
              xlab<-paste(varName)
              ylab<-paste("Bias/RMS (",unit,")")
              obPlot<-FGAnDeparture(title,xlab,ylab,plotData)
            }else{
              obPlot<-emptyPlot(title)
            }
          } else {
            # Vertical profile (Aircraft/Temp)
            obPlot<-NULL
            plotQuery<-paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,level,varname FROM obsmon ",
                             " WHERE obnumber == ",obNumber,
                             " AND dtg == ",dtg,
                             " AND obname == '",obname,"'",
	                         " AND varname == '",varName,"'",
                             " ORDER BY level",sep="")
            if ( verbosity > 0 ) {print(plotQuery)}
            title<-paste(plotName,obName,varName,dtgstr)
            plotData <- data.frame(dbGetQuery(dbConn,plotQuery))
            if ( nrow(plotData) > 0 ) {
              xlab<-paste("(",unit,")")
              ylab<-"Pressure"
              obPlot<-FGAnDepartureVert(title,xlab,ylab,plotData)
            }else{
              obPlot<-emptyPlot(title)
            }
	      }
        }else{
          print(paste(plotName,"not found!"))
          obPlot<-emptyPlot(paste(plotName,"not found!"))
        }
      }
      disconnect(dbConn)
      return(obPlot)
    }
  }

setLevelList<-function(level){

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
  ThresholdMap<-function(title,plotData){
    obPlot<-NULL

    obPlot<-ggplot(map.world,aes(long,lat))
    obPlot<-obPlot + geom_path(data=map.world, aes (group = group),colour="black",show_guide=FALSE) +
                     coord_map("stereographic",xlim=c(input$x1,input$x2),ylim=c(input$y1,input$y2)) +
                     geom_point(data=plotData,aes(x=longitude,y=latitude,colour=values),size=3) +
                     scale_colour_gradientn(colours = rainbow(15)) +
                     ylab("lat") +
                     xlab("lon") +
                     labs(title = title)
    return(obPlot)
  }
  FGAnDeparture <- function(title,xlab,ylab,plotData){
    df <- data.frame(params = factor(c("FGBias", "AnBias", "FGRMS",  "AnRMS"), c("FGBias", "AnBias", "FGRMS",  "AnRMS")), biasRMSvalues = c(plotData$fg_bias_total, plotData$an_bias_total, plotData$fg_rms_total, plotData$an_rms_total))
    obPlot <- ggplot(data=df, aes(x=params, y=biasRMSvalues, fill=c("red","darkblue","red","darkblue"))) + geom_bar(stat="identity") + guides(fill=FALSE) + ylab(ylab) + xlab(xlab)

    obPlot <- obPlot + labs(title=title)
    return(obPlot)
  }
  FGAnDepartureVert<- function(title,xlab,ylab,plotData){
     obPlot <- NULL
     lastLevel<-plotData$level[length(plotData$level)]
     if ( verbosity > 2 ) {print(plotData$level)}
     if ( verbosity > 2 ) {print(lastLevel)}
     if ( lastLevel > 9000 ) {
       obPlot <- ggplot(plotData, aes(level)) +
               geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
               geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
               geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
               geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
               geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
               geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
               geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
               geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
               coord_flip()+
               scale_x_continuous(breaks=plotData$level) +
               # Rotated beacuse of coord_flip
               ylab(xlab)+
               xlab(ylab) +
               xlim(100000,0) +
               labs(title = title)
     }else{
       obPlot <- ggplot(plotData, aes(level)) +
               geom_line(aes(y=fg_bias_total,colour="fg_bias_total")) +
               geom_point(aes(y=fg_bias_total,colour="fg_bias_total"),size=4) +
               geom_line(aes(y=an_bias_total,colour="an_bias_total"))+
               geom_point(aes(y=an_bias_total,colour="an_bias_total"),size=4) +
               geom_line(aes(y=fg_rms_total,colour="fg_rms_total")) +
               geom_point(aes(y=fg_rms_total,colour="fg_rms_total"),size=4) +
               geom_line(aes(y=an_rms_total,colour="an_rms_total")) +
               geom_point(aes(y=an_rms_total,colour="an_rms_total"),size=4) +
               coord_flip()+
               scale_x_continuous(breaks=plotData$level) +
               # Rotated beacuse of coord_flip
               ylab(xlab) +
               xlab(ylab) +
               labs(title = title)
     }
     return(obPlot)
  }
  usageMap<- function(title,plotData){
    status<-rep("NA",length(plotData$longitude))
    status<- ifelse(plotData$anflag == 0,"Rejected",status)
    status<- ifelse(plotData$active > 0,"Active",status)
#    status<- ifelse(plotData$rejected > 0,"Rejected",status)
#    status<- ifelse(plotData$passive > 0,"Passive",status)
#    status<- ifelse(plotData$blacklisted > 0,"Blacklisted",status)
    status<- ifelse(plotData$anflag > 0,"Active",status)


    plotData$status=status

#    print(plotData[rev(order(status)),])
    
    obPlot<-ggplot(map.world,aes(long,lat))
    obPlot<-obPlot + geom_path(data=map.world, aes (group = group), colour="black") +
                     coord_map("stereographic",xlim=c(input$x1,input$x2),ylim=c(input$y1,input$y2)) +
                     geom_point(data=plotData[rev(order(status)),],aes(x=longitude,y=latitude,colour=status),size=3) +
                     ylab("lat") +
                     xlab("lon") +
                     labs(title = title) +
                     scale_colour_manual(name="Legend",values=c("Active"="green","Active(2)"="blue","Rejected"="red","Passive"="yellow","Blacklisted"="black","NA"="grey"))
    return(obPlot)
  }
  SatBcorrCycle <- function(plotData,title,cycle,plotName){

    obPlot<-NULL
    if ( plotName == "Hovmoller" ){
      obPlot<-ggplot(plotData,aes(x=DTG,y=level,fill=fg_bias_total))+geom_raster()
      obPlot <- obPlot + xlab("DATE")+ylab("Channels")
      obPlot <- obPlot + labs(title=paste(title," (All cycles)"))
    }else if ( plotName == "BiasCorrection" ){
      LXX = hour(plotData$DTG)==as.integer(cycle)
      plotDataXX <- plotData[LXX,]
      obPlot <- ggplot(data=plotDataXX)
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_uncorr_total,colour="FGdep raw"))
      obPlot <- obPlot + geom_line(aes(x=DTG,y=fg_bias_total,colour="FGdep bcorr"))
      obPlot <- obPlot + xlab("DATE")+ylab("T [K]")
      obPlot <- obPlot + labs(title=paste(title,cycle," UTC"))
      obPlot <- obPlot + geom_text(aes(x=DTG,y=fg_uncorr_total,label=nobs_total))
      obPlot <- obPlot + facet_wrap(~ level)
    }
    return(obPlot)
  }

