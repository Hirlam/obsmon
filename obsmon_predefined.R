

# getGroups
getPreDefinedGroups<- function(){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> getGroups")) }

  # Predefined groups
  preDefinedGroups    <- c("MetCoOp","DMI")
  return(preDefinedGroups)
}

# getPreDefinedPlots
getPreDefinedPlots<- function(group){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> getPreDefinedPlots(",group,")")) }
  
  if (!is.null(group)) {
    switch(group,
      "MetCoOp" = { c("T2M observation usage latest cycle","Snow observation usage latest cycle","TEMP number of temperature observations last week")},
      "DMI"     = { NULL},
      NULL
    )
  } 
}

# generatePreDefinedPlot
generatePreDefinedPlot<- function(plot){
  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generatePreDefinedPlot(",plot,")")) }

  obPlot=NULL
  if ( !is.null(plot)) {
    switch(plot,
      "T2M observation usage latest cycle" = {
        base=c("Surface")
        shortPlotType=c("ObservationUsage")
        obtype=c("SYNOP")
        variable=c("t2m")
        level=c("Surface")
        sensor=NULL
        satelite=NULL
        channel=NULL
        date1=getLatestDate(base)
        date2=getLatestDate(base)
        cycle=getLatestCycle(base)
      },
      "Snow observation usage latest cycle" = {
        base=c("Surface") 
        shortPlotType=c("ObservationUsage")
        obtype=c("SYNOP")
        variable=c("snow")
        level=c("Surface")
        sensor=NULL
        satelite=NULL
        channel=NULL
        date1=getLatestDate(base)
        date2=getLatestDate(base)
        cycle=getLatestCycle(base)
      },
      "TEMP number of temperature observations last week" = {
        base=c("Minimization")
        shortPlotType=c("NumberOfObservations")
        obtype=c("TEMP")
        variable=c("t")
        level=c("ALL")
        sensor=NULL
        satelite=NULL
        channel=NULL
        date2=getLatestDate(base)
        date1=getPastDate(date2,7)
        cycle=getLatestCycle(base)
      },
      NULL
    )
    obPlot=generatePlot(base,shortPlotType,obtype,variable,level,sensor,satelite,channel,c(date1,date2),cycle)
  }
  return(obPlot)
}
