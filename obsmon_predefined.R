

# getGroups
getPreDefinedGroups<- function(){
  flog.debug("-> getGroups")

  # Predefined groups
  preDefinedGroups=default_experiments

  if ( input$showExistingDataOnly ){
    experiments=NULL
    for (n in 1:length(default_experiments)){
      bases=c("Minimization","Surface")
      not_found=TRUE
      for (j in 1:length(bases)){
        if ( not_found ){
          if ( !is.null(setExperiment(default_experiments[n],bases[j]))){
            experiments=c(experiments,default_experiments[n])
            not_found=FALSE
          }
        }
      }
    }
    # Set groups to found experiments
    if ( !is.null(experiments)) { preDefinedGroups=experiments}
  }
  return(preDefinedGroups)
}

# getPreDefinedPlots
getPreDefinedPlots<- function(group){
  flog.debug(paste("-> getPreDefinedPlots(", group, ")"))
  
  if (!is.null(group)) {
    switch(group,
      "MetCoOp" = { c("T2M observation usage latest cycle","Snow observation usage latest cycle","TEMP number of temperature observations last week")},
      "DMI" = { c("T2M observation usage latest cycle","Snow observation usage latest cycle","TEMP number of temperature observations last week")},
      "FMI" = { c("T2M observation usage latest cycle","Snow observation usage latest cycle","TEMP number of temperature observations last week")},
      { c("T2M observation usage latest cycle","Snow observation usage latest cycle")}
    )
  } 
}

# generatePreDefinedPlot
generatePreDefinedPlot<- function(plot,exp,mode="plot"){
  flog.debug(paste("-> generatePreDefinedPlot(", plot, exp, ")"))

  obPlot=NULL
  if ( !is.null(plot) && !is.null(exp)) {
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
        date2=getLatestDate(base,exp)
        date1=date2
        cycle=getLatestCycle(base,exp)
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
        date2=getLatestDate(base,exp)
        date1=date2
        cycle=getLatestCycle(base,exp)
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
        date2=getLatestDate(base,exp)
        date1=getPastDate(date2,7)
        cycle=getLatestCycle(base,exp)
      },
      NULL
    )
    obPlot=generatePlot(base,exp,shortPlotType,obtype,variable,level,sensor,satelite,channel,c(date1,date2),cycle,mode)
  }
  return(obPlot)
}
