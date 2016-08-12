
#
# General plotting interface controlling all of the plotting
# Organized in if tests for the different plot types
#
# Depending on the mode:
# Returns either a ggplot2 object, a string with the the SQL query or a data frame with the data.
#

generatePlot <- function(odbBase,exp,plotName,obName,varName,levels,sensor,satelite,channels,dateRange,cycle,mode="plot") {
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generatePlot",odbBase,exp,plotName,obName,varName,levels,sensor,satelite,channels,dateRange,cycle,mode)) }

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

  scatt_extra=""
  if ( obNumber == "9" && odbBase == "Minimization" ) { scatt_extra=" AND active == 1 " }

  obPlot=NULL

# NumberOfObservations
  if ( plotName == "NumberOfObservations"){
    if (obNumber == "7") {
      qsatelite = setDBSatname(as.character(satelite))
      channelListQuery = setChannelList(channels)
      plotQuery = paste("SELECT DTG,nobs_total,level FROM obsmon",
                        " WHERE obnumber = ",obNumber,
                        " AND DTG >= ",dtgbeg,
                        " AND DTG <= ",dtgend,
                        " AND obname == '",tolower(sensor),"'",
                        " AND satname == '",qsatelite,"'",
                        channelListQuery,sep="")
      ylab="Channel"
      title=paste(exp,":",plotName,obName,satelite,dtgstr_range)
    }else{
      levelListQuery = setLevelList(levels)
      plotQuery = paste("SELECT DTG,nobs_total,level FROM obsmon",
                        " WHERE obnumber = ",obNumber,
                        " AND DTG >= ",dtgbeg,
                        " AND DTG <= ",dtgend,
                        " AND obname == '",obname,"'",
                        " AND varname == '",varName,"'",
                        levelListQuery,sep="")
      ylab="Level"
      title = paste(exp,":",plotName,obName,varName,level_string,dtgstr_range)
    }
    if ( verbose("INFO") ) { paste("INFO: ",print(plotQuery))}
    if ( mode == "query" ) { return(plotQuery)}
    plotData=getDataTS(odbBase,exp,dtgbeg,dtgend,mode,plotQuery)
    if ( mode == "data" ) { return(plotData)}
    obPlot = NumberOfObservations(title,ylab,plotData)

# ObsFitTs
    } else if ( plotName ==  "ObsFitTs" ) {

      if (obNumber == "7") {
        qsatelite = setDBSatname(as.character(satelite))
        channelListQuery = setChannelList(channels)
        plotQuery = paste("SELECT DTG,nobs_total,level,an_bias_total,fg_bias_total,an_rms_total,fg_rms_total FROM obsmon",
                          " WHERE obnumber = ",obNumber,
                          " AND DTG >= ",dtgbeg,
                          " AND DTG <= ",dtgend,
                          " AND obname == '",tolower(sensor),"'",
                          " AND satname == '",qsatelite,"'",
                          channelListQuery,sep="")
        ylab="Channel"
        title=paste(exp,":",plotName,obName,satelite,dtgstr_range)
      }else{

        levelListQuery = setLevelList(levels)
        plotQuery = paste("SELECT DTG,nobs_total,level,an_bias_total,fg_bias_total,an_rms_total,fg_rms_total FROM obsmon",
                          " WHERE obnumber = ",obNumber,
                          " AND DTG >= ",dtgbeg,
                          " AND DTG <= ",dtgend,
                          " AND obname == '",obname,"'",
                          " AND varname == '",varName,"'",
                          levelListQuery,sep="")
        ylab=getUnit(varName)
        title = paste(exp,":",plotName,obName,varName,level_string,dtgstr_range)

      }

      if ( verbose("INFO") ) { paste("INFO: ",print(plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      plotData=getDataTS(odbBase,exp,dtgbeg,dtgend,mode,plotQuery)
      if ( mode == "data" ) { return(plotData)}
      obPlot = ObsFitTs(title,ylab,plotData)

# FirstGuessDepartureMap
# FirstGuessBCDepartureMap
# AnalysisDepartureMap
# AnalysisIncrementMap
# BiasCorrectionMap
# ObservationsMap
  } else if ( plotName == "FirstGuessDepartureMap" || plotName == "FirstGuessBCDepartureMap" || plotName == "AnalysisDepartureMap" || plotName == "AnalysisIncrementMap" || plotName == "BiasCorrectionMap" || plotName == "ObservationsMap" ) {
    if ( plotName == "FirstGuessDepartureMap" ){
      sql = "(fg_dep) as plotValues"
    }else if ( plotName == "FirstGuessBCDepartureMap" ){
      sql = "(fg_dep+biascrl) as plotValues"
    }else if ( plotName == "AnalysisDepartureMap" ){
      sql = "(an_dep) as plotValues"
    }else if ( plotName == "AnalysisIncrementMap" ){
      sql = "(fg_dep-an_dep) as plotValues"
    }else if ( plotName == "BiasCorrectionMap") {
      sql = "(biascrl) as plotValues"
    }else if ( plotName == "ObservationsMap") {
      sql = "(obsvalue) as plotValues"
    }

    ff=""
    if (obNumber == "7") {
      qsatelite = setDBSatname(as.character(satelite))
      channelListQuery = setChannelList(channels)
      plotQuery = paste("SELECT latitude,longitude,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",tolower(sensor),"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
      title = paste(exp,":",plotName,obName,satelite,channel_string,dtgstr)
    }else{
      levelListQuery = setLevelList(levels)
      if ( varName == "ff10m" ){
        ff="ff10m"
        var1="u10m"
        var2="v10m"
      }
      if ( varName == "ff" ){
        ff="ff"
        var1="u"
        var2="v"
      }
      if ( ff != "" ){
        plotQuery = paste("SELECT latitude,longitude,level,statid,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",var1,"'",
                           scatt_extra,
                           levelListQuery,sep="")
                           dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
                           plotData1 = data.frame(dbGetQuery(dbConn,plotQuery))
                           disconnect(dbConn)
        plotQuery = paste("SELECT latitude,longitude,level,statid,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",var2,"'",
                           scatt_extra,
                           levelListQuery,sep="")
      }else{
        plotQuery = paste("SELECT latitude,longitude,level,statid,",sql," FROM usage",
                           " WHERE obnumber = ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           scatt_extra,
                           levelListQuery,sep="")
      }
      title = paste(exp,":",plotName,obName,varName,level_string,dtgstr)
    }
    if ( verbose("INFO") ) { paste("INFO: ",print(plotQuery))}
    if ( mode == "query" ) { return(plotQuery)}
    dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
    plotData = data.frame(dbGetQuery(dbConn,plotQuery))
    disconnect(dbConn)
    #plotData$scale = "reverse"
    plotData$scale = "normal"
    if ( plotName == "BiasCorrectionMap") {
      plotData$scale = "normal"
    }else if ( plotName == "AnalysisIncrementMap" ){
      plotData$scale = "normal"
    }else if ( plotName == "ObservationsMap") {
      plotData$scale = "obs"
    }
    if ( ff != "" ){
      plotData2=plotData
      plotData$plotValues=sqrt((plotData1$plotValues*plotData1$plotValues)+(plotData2$plotValues*plotData2$plotValues))
    }
    if ( mode == "data" ) { return(plotData)}
    if ( nrow(plotData) > 0 ) {
      if ( ff != "" ){
        obPlot = ThresholdMap(title,plotData,mode,odbBase,exp,dtg,u=plotData1$plotValues,v=plotData2$plotValues)
      }else{
        obPlot = ThresholdMap(title,plotData,mode,odbBase,exp,dtg)
      }
    }else{
      obPlot = emptyPlot(title)
    }

  } else if ( plotName == "LandSeaDepartures" ){
    obPlot=NULL
    if ( obNumber == "7" ){
      qsatelite = setDBSatname(as.character(satelite))
      channelListQuery = setChannelList(channels)
      plotQuery = paste("SELECT DTG,fg_dep_land,fg_dep_sea,an_dep_land,an_dep_sea,fg_uncorr_land,fg_uncorr_sea,nobs_land,nobs_sea,level FROM obsmon",
                        " WHERE obnumber = ",obNumber,
                        " AND DTG >= ",dtgbeg,
                        " AND DTG <= ",dtgend,
                        " AND obname == '",tolower(sensor),"'",
                        " AND satname == '",qsatelite,"'",
                        channelListQuery,sep="")
      if ( verbose("INFO") ) { paste("INFO: ",print(plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      plotData=getDataTS(odbBase,exp,dtgbeg,dtgend,mode,plotQuery)
      if ( mode == "data" ) { return(plotData)}
      title=paste(exp,":",plotName,obName,satelite,dtgstr_range)
      obPlot = LandSeaDepartures(title,plotData)
    } 
# ObservationUsage
  }else if ( plotName == "ObservationUsage" ) {
    # SATEM
    if (obNumber == "7") {

      qsatelite = setDBSatname(as.character(satelite))
      channelListQuery = setChannelList(channels)
      plotQuery = paste("SELECT latitude,longitude,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND DTG == ",dtg,
                           " AND obname == '",tolower(sensor),"'",
                           " AND satname == '",qsatelite,"'",
                           channelListQuery,sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      title = paste(exp,":",plotName,sensor,satelite,channel_string,dtgstr)
      dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
      plotData = dbGetQuery(dbConn,plotQuery)
      disconnect(dbConn)
      if ( mode == "data" ) { return(plotData)}
      if ( nrow(plotData) > 0 ) {
        obPlot = usageMap(title,plotData,mode)
      }else{
        obPlot = emptyPlot(title)
      }
    } else {
      # Non SATEMs
      levelListQuery = setLevelList(levels)
      obPlot = NULL
      plotQuery = paste("SELECT latitude,longitude,statid,active,rejected,passive,blacklisted,anflag FROM usage",
                           " WHERE obnumber == ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                          scatt_extra,
                          levelListQuery,sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      title = paste(exp,":",plotName,obName,varName,level_string,dtgstr)
      dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
      plotData = dbGetQuery(dbConn,plotQuery)
      disconnect(dbConn)
      if ( mode == "data" ) { return(plotData)}
      if ( nrow(plotData) > 0 ) {
        obPlot = usageMap(title,plotData,mode)
      }else{
        obPlot = emptyPlot(title)
      }
    }
# Bias correction time-series
  } else if ( plotName == "BiasCorrection" || plotName == "Hovmoller" ) {
    # SATEM
    if (obNumber == "7") {
      qsatelite = setDBSatname(as.character(satelite))
      channelListQuery = setChannelList(channels)
      plotQuery = paste("SELECT DTG,fg_bias_total,fg_uncorr_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND obname == '",tolower(sensor),"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG >= ",dtgbeg,
                             " AND DTG <= ",dtgend,
                             channelListQuery,sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      if ( plotName == "BiasCorrection" ){
        plotData=getDataTS(odbBase,exp,dtgbeg,dtgend,mode,plotQuery,cycle)
      }else{
        plotData=getDataTS(odbBase,exp,dtgbeg,dtgend,mode,plotQuery)
      }
      title = paste(exp,":",plotName,satelite,sensor,channel_string,dtgstr_range,sep=" ")
      if ( mode == "data" ) { return(plotData)}
      if ( nrow(plotData) > 0 ) {
        obPlot = SatBcorrCycle(title,cycle,plotName,plotData)
      }else{
        obPlot = emptyPlot(title)
      }
    # Non-SATEMSs do not have bias correction from ODB
    } else {
       obPlot=NULL
    }
# FGAnDeparture
  } else if ( plotName == "FGAnDeparture" ) {
    # SATEM
    if (obNumber == "7") {
      qsatelite = setDBSatname(as.character(satelite))
      plotQuery = paste("SELECT DTG,fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,nobs_total,level FROM obsmon",
                             " WHERE obnumber ==",obNumber,
                             " AND obname == '",tolower(sensor),"'",
                             " AND satname == '",qsatelite,"'",
                             " AND DTG == ",dtg,
                             " AND level>=1 AND level<=10000",
                             " ORDER BY level",sep="")
      if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
      if ( mode == "query" ) { return(plotQuery)}
      dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
      title = paste(exp,":",plotName,sensor,satelite,dtgstr)
      plotData = data.frame(dbGetQuery(dbConn,plotQuery))
      disconnect(dbConn)
      if ( mode == "data" ) { return(plotData)}
      if ( nrow(plotData) > 0 ) {
        ylab = "Channels"
        xlab = "Brightness temperature [K]"
        obPlot = FGAnDepartureVert(title,xlab,ylab,plotData)
      }else{
        obPlot = emptyPlot(title)
      }
    # Non-SATEMs
    }else{
      # Surface
      if ( obNumber == 1 || obNumber == 4 || obNumber == 9 ) {
        if ( verbose("DEBUG") ) { print(paste("DEBUG: ",obname))}
        plotQuery = paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total FROM obsmon",
                             " WHERE obnumber == ",obNumber,
                             " AND dtg == ",dtg,
                             " AND obname == '",obname,"'",
                             " AND varname == '",varName,"'",sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery)) }
        if ( mode == "query" ) { return(plotQuery)}
        dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
        title = paste(exp,":",plotName,obName,varName,dtgstr)
        plotData = data.frame(dbGetQuery(dbConn,plotQuery))
        disconnect(dbConn)
        if ( mode == "data" ) { return(plotData)}
        if ( nrow(plotData) > 0 ) {
          title = paste(exp,":",plotName,obName,varName,dtgstr)
          xlab = paste(varName)
          ylab = paste("Bias/RMS (",unit,")")
          obPlot = FGAnDeparture(title,xlab,ylab,plotData)
        }else{
          obPlot = emptyPlot(title)
        }
      } else {
        # Vertical profile (Aircraft/Temp)
        plotQuery = paste("SELECT fg_bias_total,an_bias_total,fg_rms_total,an_rms_total,level,varname,nobs_total FROM obsmon ",
                           " WHERE obnumber == ",obNumber,
                           " AND dtg == ",dtg,
                           " AND obname == '",obname,"'",
                           " AND varname == '",varName,"'",
                           " ORDER BY level",sep="")
        if ( verbose("INFO") ) { print(paste("INFO: ",plotQuery))}
        if ( mode == "query" ) { return(plotQuery)}
        dbConn = connect(odbBase,exp,date2dtg(dateRange[1],cycle))
        title = paste(exp,":",plotName,obName,varName,dtgstr)
        plotData = data.frame(dbGetQuery(dbConn,plotQuery))
        disconnect(dbConn)
        if ( mode == "data" ) { return(plotData)}
        if ( nrow(plotData) > 0 ) {
          xlab = paste("(",unit,")")
          ylab = "Pressure"
          obPlot = FGAnDepartureVert(title,xlab,ylab,plotData)
        }else{
          obPlot = emptyPlot(title)
        }
      }
    }
  }else{
    if ( verbose("WARNING")) { print(paste("WARNING: ",plotName,"not found!")) }
    obPlot = emptyPlot(paste(exp,":",plotName,"not found!"))
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
      image = melt(image)
      names(image) = c('row','column','fill')
      plot = qplot(column, -row, data = image, geom = 'tile', fill = fill) +
        scale_fill_gradient(low = 'black', high = 'white')
    }
  
    if(length(dim(image)) == 3){
  	  message('creating color image...')
  	  image = apply(image, 1:2, function(v) rgb(v[1], v[2], v[3]))
      image = melt(image)
      names(image) = c('row', 'column', 'fill')
      plot = qplot(column, -row, data = image, geom = 'tile', fill = fill) +
      scale_fill_identity()  	
    }
  }
  # Empty plot function
  emptyPlot <- function(title){
    image = readJPEG("./nodata.jpg")
    obPlot = ggimage(image) + coord_equal() + labs(title=title)
    return(obPlot)
  }

  # ThresholdMap
  ThresholdMap <- function(title,plotData,mode,base,exp,dtg,u=NULL,v=NULL){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> ThresholdMap",title,mode,base,exp,dtg)) }

    obPlot = NULL

    x1=min(plotData$longitude)-2
    x2=max(plotData$longitude)+2
    y1=min(plotData$latitude)-2
    y2=max(plotData$latitude)+2

    if ( mode == "map" ) {
      zoomLevel=4
      zoomLevels=c(2000,4000,6000,10000,15000,45000,70000,90000)
      for ( i in 2:length(zoomLevels)){
        if ((length(plotData$plotValues) > zoomLevels[i-1]) && (length(plotData$plotValues) <= zoomLevels[i])){
          zoomLevel=i+3
          if ( verbose("DEBUG") ) { print(paste("DEBUG: -> zoomLevel=",zoomLevel)) }
        }
      }
   
      r = getRasterFromFile(base,exp,dtg)
      if ( !is.null(r)) {
        dmin2=min(values(r))
        dmax2=max(values(r))
        ulim2=max(abs(dmin2),abs(dmax2))
        pal2 <- colorNumeric(c("#0000FF", "#FFFFFF", "#FF0000"),domain=c(-ulim2,ulim2),na.color = "transparent")
      }
      #dmin <- quantile(plotData$plotValues,c(0.01),type=3,names=F)
      #dmax <- quantile(plotData$plotValues,c(0.99),type=3,names=F)
      dmin = min(plotData$plotValues)
      dmax = max(plotData$plotValues)
      ulim <- max(abs(dmin),abs(dmax))
      if (plotData$scale[1] == "reverse") {
        pal <- colorNumeric(palette=c("#FF0000","#FFFFFF","#0000FF"),domain=c(-ulim,ulim))
      } else if (plotData$scale[1] == "normal") {
        pal <- colorNumeric(palette=c("#0000FF","#FFFFFF","#FF0000"),domain=c(-ulim,ulim))
      } else if (plotData$scale[1] == "obs") {
        if (dmin*dmax < 0) {  # both signs, normal scale
          pal <- colorNumeric(palette=c("#FF0000","#FFFFFF","#0000FF"),domain=c(-ulim,ulim))
        } else if (dmin < 0) {  # all negative, blue to white
          pal <- colorNumeric(palette=c("#0000FF","#FFFFFF"),domain=c(dmin,dmax))
        } else {  # all positive, white to red
          pal <- colorNumeric(palette=c("#FFFFFF","#FF0000"),domain=c(dmin,dmax))
        }
      }
      plotData$popup = paste("Statid: ",plotData$statid,"<br>Value: ",signif(plotData$plotValues,digits=5),"<br>Level: ",plotData$level)
      if ( max(plotData$plotValues) > 0 ) {
        plotData$radius=(abs(plotData$plotValues)/max(abs(plotData$plotValues)))*10
        if ( length(plotData$radius) > 0 ) {
          for (i in 1:length(plotData$radius)) {
            if ( plotData$radius[i] < 3 ){ plotData$radius[i]=3}
            #if ( plotData$radius[i] >10 ){ plotData$radius[i]=10}
          }
        }
      }else{
        plotData$radius=5
      }
      if ( is.null(r)){
        obMap <- leaflet(plotData) %>%
          addProviderTiles("Esri.WorldStreetMap", options=providerTileOptions(opacity=0.7)) %>%
            fitBounds(x1, y1, x2, y2) %>%
              addCircleMarkers(~longitude, ~latitude, popup=~popup, radius=~radius,
                             stroke=TRUE, weight=1, opacity=1, color="black",
                             fillColor=~pal(plotValues), fillOpacity=1,clusterOptions = markerClusterOptions(disableClusteringAtZoom=zoomLevel)) %>%
                             addLegend("topright", pal=pal, values=~plotValues, opacity=1)
      }else{
        obMap <- leaflet(plotData) %>%
          addProviderTiles("Esri.WorldStreetMap", options=providerTileOptions(opacity=1.0)) %>%
            fitBounds(x1, y1, x2, y2) %>%
              addCircleMarkers(~longitude, ~latitude, popup=~popup, radius=~radius,
                             stroke=TRUE, weight=1, opacity=1, color="black",
                             fillColor=~pal(plotValues), fillOpacity=1,clusterOptions = markerClusterOptions(disableClusteringAtZoom=zoomLevel)) %>%
                             addLegend("topright", pal=pal, values=~plotValues, opacity=1) %>%
          addRasterImage(r, colors = pal2, opacity = 0.6) %>%
                             addLegend("bottomright",pal = pal2, values = values(r))
      }
      return(obMap)
    } else {

      obPlot = ggplot(map.world,aes(long,lat))
      obPlot = obPlot + geom_path(data=map.world, aes (group = group),colour="black",show_guide=FALSE) +
                       coord_map("stereographic",xlim=c(x1,x2),ylim=c(y1,y2)) +
                       geom_point(data=plotData,aes(x=longitude,y=latitude,colour=plotValues),size=3) +
                       scale_colour_gradientn(colours = rainbow(15)) +
                       ylab("lat") +
                       xlab("lon") +
                       labs(title = title)
      if ( !is.null(u) && !is.null(v) ){
        plotData$u=-u
        plotData$v=-v
        plotData$un=plotData$u/max(plotData$plotValues)
        plotData$vn=plotData$v/max(plotData$plotValues)
        obPlot = obPlot + geom_segment(data=plotData,aes(x=longitude,y=latitude,xend=longitude+un,yend=latitude+vn))
      }
      return(obPlot)
    }
  }

  # FGAnDeparture
  FGAnDeparture <- function(title,xlab,ylab,plotData){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDeparture",title,xlab,ylab)) }

    df = data.frame(params = factor(c("FGBias", "AnBias", "FGRMS",  "AnRMS"), c("FGBias", "AnBias", "FGRMS",  "AnRMS")), biasRMSvalues = c(plotData$fg_bias_total, plotData$an_bias_total, plotData$fg_rms_total, plotData$an_rms_total))
    obPlot = ggplot(data=df, aes(x=params, y=biasRMSvalues, fill=c("red","darkblue","red","darkblue"))) + geom_bar(stat="identity") + guides(fill=FALSE) + ylab(ylab) + xlab(xlab)

    obPlot = obPlot + labs(title=title)
    return(obPlot)
  }

  # FGAnDepartureVert
  FGAnDepartureVert <- function(title,xlab,ylab,plotData){
     if ( verbose("DEBUG") ) { print(paste("DEBUG: -> FGAnDepartureVert",title,xlab,ylab)) }
     obPlot = NULL
     lastLevel = plotData$level[length(plotData$level)]
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",plotData$level))}
     if ( verbose("DEBUG") ) { print(paste("DEBUG: ",lastLevel))}
     if ( lastLevel > 90000 ) {

         DTG=plotData$DTG
         level=plotData$level
         RMS_FGdep=plotData$fg_rms_total
         RMS_ANdep=plotData$an_rms_total
         BIAS_FGdep=plotData$fg_bias_total
         BIAS_ANdep=plotData$an_bias_total
         NROBS=plotData$nobs_total

         testData=data.frame(level,RMS_FGdep,RMS_ANdep,BIAS_FGdep,BIAS_ANdep)
         localPlotData <- melt(testData,id=c("level"))

         obPlot = ggplot(data=localPlotData,aes(x=level,y=value,group=variable,colour=variable,shape=variable))
         obPlot = obPlot + geom_line() + geom_point(size=4) +
                    scale_shape_manual(values=c(16,16,32,32)) +
                    scale_colour_manual(values=c("blue", "red", "blue", "red")) +
                    coord_flip()+
                    # Rotated beacuse of coord_flip
                    ylab(xlab) + xlab(ylab) + 
                    xlim(100000,0) + 
                    labs(title = title)

     }else{
       obPlot = ggplot(plotData, aes(level)) +
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

  # usageMap
  usageMap <- function(title,plotData,mode){
    status = rep("NA",length(plotData$longitude))
    status = ifelse(plotData$anflag == 0,"Rejected",status)
    status = ifelse(plotData$active  > 0,"Active",status)
    status = ifelse(plotData$rejected > 0,"Rejected",status)
    status = ifelse(plotData$passive > 0,"Passive",status)
    status = ifelse(plotData$blacklisted > 0,"Blacklisted",status)
    status = ifelse(plotData$anflag  > 0,"Active(2)",status)
    status = ifelse(plotData$anflag == 4,"Rejected",status)
    status = ifelse(plotData$anflag == 8,"Blacklisted",status)
    obPlot = NULL

    plotData$status=status

    x1=min(plotData$longitude)-2
    x2=max(plotData$longitude)+2
    y1=min(plotData$latitude)-2
    y2=max(plotData$latitude)+2
 
    if ( mode == "map" ) {
      zoomLevel=4
      zoomLevels=c(2000,4000,6000,10000,15000,45000,70000,90000)
      for ( i in 2:length(zoomLevels)){
        if ((length(plotData$status) > zoomLevels[i-1]) && (length(plotData$status) <= zoomLevels[i])){
          zoomLevel=i+3
          if ( verbose("DEBUG") ) { print(paste("DEBUG: -> zoomLevel=",zoomLevel)) }
        }
      }
      pal <- colorFactor(c("green","blue","black","grey","yellow","red"),
                         domain=c("Active","Active(2)","Blacklisted","NA","Passive","Rejected"))
      plotData$popup = paste("Statid: ",plotData$statid,"<br>Anflag: ",plotData$anflag,"<br>Status:",plotData$status)
      obMap <- leaflet(data=plotData[rev(order(status)),]) %>%
        addProviderTiles("Esri.WorldStreetMap", options=providerTileOptions(opacity=0.5)) %>%
          fitBounds(x1, y1, x2, y2 ) %>%
            addCircleMarkers(~longitude, ~latitude, popup=~popup, radius=8,
                             stroke=TRUE, weight=1, opacity=1, color="black",
                             fillColor=~pal(status), fillOpacity=1,clusterOptions = markerClusterOptions(disableClusteringAtZoom=zoomLevel)) %>%
                               addLegend("topright", pal=pal, values=~status, opacity=1)
      return(obMap)
    } else {
      obPlot = ggplot(map.world,aes(long,lat))
      obPlot = obPlot + geom_path(data=map.world, aes (group = group), colour="black") +
        coord_map("stereographic",xlim=c(x1,x2),ylim=c(y1,y2)) +
          geom_point(data=plotData[rev(order(status)),],aes(x=longitude,y=latitude,colour=status),size=3) +
            ylab("lat") +
              xlab("lon") +
                labs(title = title) +
                  scale_colour_manual(name="Legend",values=c("Active"="green","Active(2)"="blue","Rejected"="red","Passive"="yellow","Blacklisted"="black","NA"="grey"))
      return(obPlot)
    }
  }

  # SatBcorrCycle
  SatBcorrCycle <- function(title,cycle,plotName,plotData){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> SatBcorrCycle",title,cycle,plotName)) }

    if ( plotName == "Hovmoller" ){
      localPlotData=plotData
      localPlotData$datetime = chron(dates=dtg2date(plotData$DTG),times=paste(dtg2utc(plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))

      obPlot = ggplot(localPlotData,aes(x=datetime,y=level,fill=fg_bias_total))+geom_raster()
      obPlot = obPlot + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
      obPlot = obPlot + xlab("DATE")+ylab("Channels")
      obPlot = obPlot + labs(title=paste(title," (All cycles)"))
    }else if ( plotName == "BiasCorrection" ){
      LXX = as.integer(dtg2utc(plotData$DTG))==as.integer(cycle)
      #print(LXX)
      plotDataXX = plotData[LXX,]
      localPlotDataXX=plotDataXX
      localPlotDataXX$datetime = chron(dates=dtg2date(plotDataXX$DTG),times=paste(dtg2utc(plotDataXX$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))
 
      obPlot = ggplot(data=localPlotDataXX)
      obPlot = obPlot + geom_line(aes(x=DTG,y=fg_uncorr_total,colour="FGdep raw"))
      obPlot = obPlot + geom_line(aes(x=DTG,y=fg_bias_total,colour="FGdep bcorr"))
      obPlot = obPlot + xlab("DATE")+ylab("T [K]")
      obPlot = obPlot + labs(title=paste(title,cycle," UTC"))
      obPlot = obPlot + geom_text(aes(x=DTG,y=fg_uncorr_total,label=nobs_total))
      obPlot = obPlot + facet_wrap(~ level)
    }
    return(obPlot)
  }

  # NumberOfObservations
  NumberOfObservations <- function(title,ylab,plotData){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> NumberOfObservations",title)) }

    localPlotData=plotData
    if ( nrow(localPlotData) > 0 ) {
        localPlotData$DTG <- paste0(substr(localPlotData$DTG,1,4),"-",
                                    substr(localPlotData$DTG,5,6),"-",
                                    substr(localPlotData$DTG,7,8)," ",
                                    substr(localPlotData$DTG,9,10),":00")
        localPlotData$DTG <- as.POSIXct(localPlotData$DTG,"%Y-%m-%d %h:%m")
    }else{
        obPlot<-emptyPlot(title)
    }
    
    obPlot = ggplot(data=localPlotData)
    obPlot = obPlot+geom_line(aes(x=DTG,y=nobs_total))
    obPlot = obPlot + xlab("DATE")+ylab("nrobs")
    obPlot = obPlot + labs(title=title,ylab=ylab)
    obPlot = obPlot + facet_wrap(~ level)
    
    return(obPlot) 
  }

  # ObsFitTs
  ObsFitTs <- function(title,ylab,plotData){
    if ( verbose("DEBUG") ) { print(paste("DEBUG: -> ObsFitTs",title)) }

    localPlotData=plotData

    if ( nrow(localPlotData) > 0 ) {
        localPlotData$DTG <- paste0(substr(localPlotData$DTG,1,4),"-",
                                    substr(localPlotData$DTG,5,6),"-",
                                    substr(localPlotData$DTG,7,8)," ",
                                    substr(localPlotData$DTG,9,10),":00")
        localPlotData$DTG <- as.POSIXct(localPlotData$DTG,"%Y-%m-%d %h:%m")
    }else{
        obPlot<-emptyPlot(title)
    }

    DTG=localPlotData$DTG
    level=localPlotData$level
    RMS_FGdep=localPlotData$fg_rms_total
    RMS_ANdep=localPlotData$an_rms_total
    BIAS_FGdep=localPlotData$fg_bias_total
    BIAS_ANdep=localPlotData$an_bias_total

    testData=data.frame(DTG,level,RMS_FGdep,RMS_ANdep,BIAS_FGdep,BIAS_ANdep)
    localPlotData <- melt(testData,id=c("DTG","level"))

    obPlot = ggplot(data=localPlotData,aes(x=DTG,y=value,group=variable,colour=variable,shape=variable))
    obPlot = obPlot + geom_line() + geom_point(size=2) +
        scale_shape_manual(values=c(16,16,32,32)) +
            scale_colour_manual(values=c("blue", "red", "blue", "red"))

    obPlot = obPlot + xlab("DATE")+ylab(ylab)
    obPlot = obPlot + labs(title=title,ylab=ylab)
    obPlot = obPlot + facet_wrap(~ level)

    return(obPlot)
  }

  # LandSeaDepartures
  LandSeaDepartures <- function(title,plotData){
     if ( verbose("DEBUG") ) { print(paste("DEBUG: -> LandSeaDepartures",title)) }

     localPlotData=plotData
     localPlotData$DTG <- paste0(substr(localPlotData$DTG,1,4),"-",
                                    substr(localPlotData$DTG,5,6),"-",
                                    substr(localPlotData$DTG,7,8)," ",
                                    substr(localPlotData$DTG,9,10),":00")
     localPlotData$DTG <- as.POSIXct(localPlotData$DTG,"%Y-%m-%d %h:%m")

     DTG=localPlotData$DTG
     level=localPlotData$level
     omf_sea=localPlotData$fg_dep_sea
     oma_sea=localPlotData$an_dep_sea
     omfnc_sea=localPlotData$fg_uncorr_sea
     omf_land=localPlotData$fg_dep_land
     oma_land=localPlotData$an_dep_land
     omfnc_land=localPlotData$fg_uncorr_land
     nobs_sea=localPlotData$nobs_sea
     nobs_land=localPlotData$nobs_land

     testDataTop=data.frame(DTG,level,omfnc_sea,omf_sea,oma_sea,omfnc_land,omf_land,oma_land)
     localPlotDataTop=melt(testDataTop,id=c("DTG","level"))
     print(localPlotDataTop)

     testDataBottom=data.frame(DTG,level,nobs_sea,nobs_land)
     localPlotDataBottom=melt(testDataBottom,id=c("DTG","level"))
     print(localPlotDataBottom)

     ylab="Brigthness temperature"
     names=c("FG_DEP_NC (sea)", "FG_DEP (sea)","AN_DEP (sea)","FG_DEP_NC (land)", "FG_DEP (land)","AN_DEP (land)")
     colours=c("blue", "blue", "blue", "green","green","green")
     linetypes=c("dotted","solid","dashed","dotted","solid","dashed")
     shapes=c(2,1,0,2,1,0)
     obPlot = ggplot(data=localPlotDataTop,aes(x=DTG,y=value,group=variable,colour=variable,linetype=variable,shape=variable))
     obPlot = obPlot + geom_line() + geom_point(fill = "white",size=2)
     obPlot = obPlot + scale_colour_manual(name="",labels=names,values=colours) +
                  scale_linetype_manual(name="",labels=names,values=linetypes) +
                  scale_shape_manual(name="",labels=names,values=shapes)
     obPlot = obPlot + geom_hline(yintercept = 0.2)
     obPlot = obPlot + geom_hline(yintercept = -0.2)
     obPlot = obPlot + xlab("DATE")+ylab(ylab)
     obPlot = obPlot + labs(title=title,ylab=ylab)
     obPlot = obPlot + facet_wrap(~ level)
     top=obPlot

     ylab="Number of obs."
     title=""
     names=c("# obs (sea)", "# obs (land)")
     obPlot = ggplot(data=localPlotDataBottom,aes(x=DTG,y=value,group=variable,fill=variable))
     obPlot = obPlot + geom_bar(stat="identity", position=position_dodge()) + scale_fill_manual(name="",labels=names,values=c("blue", "green"))
     obPlot = obPlot + xlab("DATE")+ylab(ylab)
     obPlot = obPlot + labs(title=title,ylab=ylab)
     obPlot = obPlot + facet_wrap(~ level)
     bottom=obPlot

     obPlot = grid.arrange(top, bottom, ncol=1)
     return(obPlot)
   }

generate_surfdia <- function(var,station,exp,mode="plot"){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generate_surfdia",var,station,exp,mode)) }

  if ( !is.null(var) && !is.null(station)){

    base=NULL
    switch(var,"U10M" = { base="Minimization"}, "V10M" = { base="Minimization"},"APD" = { base="Minimization"},"Z" = { base="Minimization"},{ base="Surface"})

    dtg2=getLatestDTG(base,exp)
    date2=dtg2date(dtg2)
    cycle=dtg2utc(dtg2)
    date1=getPastDate(date2,input$ndays)
    cycle=getLatestCycle(base,exp)
    dtg1=date2dtg(date1,cycle)

    stationstr=strsplit(station,'\\[')
    station2=gsub('\\]','',stationstr[[1]][2])

    obPlot=NULL
    plotQuery = paste("SELECT dtg,obsvalue,fg_dep,an_dep,biascrl,statid FROM usage ",
                             " WHERE statid LIKE '%",station2,"%'",
                             " AND DTG >= ",dtg1," AND DTG <= ",dtg2,
                             " AND obname == 'synop' ",
                             " AND varname == '",tolower(var),"'",sep="")

    plotData=getDataTS(base,exp,dtg1,dtg2,mode,plotQuery)

    title = paste(exp,var,station)
    if ( mode == "query" ) { return(plotQuery)}
    if ( mode == "data" ) { return(plotData)}
    if ( nrow(plotData) > 0 ) {
      if ( var == "APD" ) {

        plotData$datetime = chron(dates=dtg2date(plotData$DTG),times=paste(dtg2utc(plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))
      
        obPlot = ggplot(plotData,aes(x=datetime,y=obsvalue),group="")
        obPlot = obPlot + geom_line(aes(y=obsvalue,colour="Obs",group=""))
        obPlot = obPlot + geom_line(aes(y=obsvalue+biascrl,colour="Obs raw",group=""))
        obPlot = obPlot + geom_line(aes(y=obsvalue-fg_dep,colour="FG",group=""))
        obPlot = obPlot + geom_line(aes(y=obsvalue-an_dep,colour="AN",group=""))
        obPlot = obPlot + xlab("DATE") + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
        obPlot = obPlot + scale_colour_manual(values=c("black","blue","green","red"))
        obPlot = obPlot + labs(title=title,ylab=ylab)
      }else{
        plotData$datetime = chron(dates=dtg2date(plotData$DTG),times=paste(dtg2utc(plotData$DTG),":00:00",sep=""),format=c('y-m-d','h:m:s'))      
        obPlot = ggplot(plotData,aes(x=datetime,y=obsvalue),group="")
        obPlot = obPlot + geom_line(aes(y=obsvalue,colour="Obs",group=""))
        obPlot = obPlot + geom_line(aes(y=obsvalue-fg_dep,colour="FG",group=""))
        obPlot = obPlot + geom_line(aes(y=obsvalue-an_dep,colour="AN",group=""))
        obPlot = obPlot + xlab("DATE") + scale_x_continuous(label=function(datetime) strftime(chron(datetime), "%Y-%m-%d"))
        obPlot = obPlot + scale_colour_manual(values=c("black","green","red"))
        obPlot = obPlot + labs(title=title,ylab=ylab)
      }

      maxval=max(plotData$fg_dep,plotData$an_dep)
      minval=min(plotData$fg_dep,plotData$an_dep)
      bw=(maxval-minval)/20

      bottom1 = ggplot(plotData)
      bottom1 = bottom1 + geom_histogram(aes(x=fg_dep),colour = "black", fill = "red", binwidth = bw)
      bottom1 = bottom1 + geom_vline(xintercept = 0.0)

      bottom2 = ggplot(plotData)
      bottom2 = bottom2 + geom_histogram(aes(x=an_dep),colour = "black", fill = "green", binwidth = bw)
      bottom2 = bottom2 + geom_vline(xintercept = 0.0)
      bottom = grid.arrange(bottom1, bottom2, ncol=2)

      obPlot = grid.arrange(obPlot, bottom1, bottom2,  ncol=1)
    }
    return(obPlot)
  }else{
    return(NULL)
  }
}

