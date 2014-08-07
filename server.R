# Define server logic required to plot various variables

require(shiny)
require(RSQLite)
library(ggplot2)
require(jpeg)
require(reshape2)

widthOfPlot=800
map.world<-map_data(map="world")

values<-reactiveValues()
values$plotData=NULL
values$plotQuery=NULL

shinyServer(function(input,output) {

  # Source functions
  source("./obsmon_functions.R",local=TRUE)
  source("./obsmon_plots.R",local=TRUE)

  # getData (Checks for file uploading/selection)
  getData <- reactive({
    if(!is.null(input$ODBbase_screening)){
      print("DEBUG: -> getData ODBbase_screening")
    }
    if(!is.null(input$ODBbase_minimization)){
      print("DEBUG: -> getData ODBbase_minimization")
    }
    if(!is.null(input$ODBbase_surface)){
      print("DEBUG: -> getData ODBbase_surface")
    } 
  })
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  ###############################################################################################
  # UI
  ###############################################################################################

  ####################### Selects ############################
  # select_date
  output$select_date <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_date") }
    if ( is.null(input$base)) {
        dateRangeInput("dateRange",
        label = h5("Date range"),
        start = getLatestDate("Screening"),
        end   = getLatestDate("Screening")
      )
    }else{
      dateRangeInput("dateRange",
        label = h5("Date range"),
        start = getLatestDate(input$base),
        end   = getLatestDate(input$base)
      )
    }
  })
  # select_date_SA
  output$select_date_SA <- renderUI({
    if ( verbose("DEBUG")) { print("DEBUG: -> select_date_SA") }
    if ( is.null(input$base)) {
        dateRangeInput("dateRange_SA",
        label = h5("Date range"),
        start = getLatestDate("Screening"),
        end   = getLatestDate("Screening")
      )
    }else{
      dateRangeInput("dateRange_SA",
        label = h5("Date range"),
        start = getLatestDate(input$base_SA),
        end   = getLatestDate(input$base_SA)
      )
    }
  })
  # select_cycle
  output$select_cycle <- renderUI({
    if ( verbose("DEBUG")) { print("DEBUG: -> select_cycle") }
    if ( is.null(input$base)) {
      selectInput("cycle",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"),selected = getLatestCycle("Screening")
      )
    }else{
      selectInput("cycle",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"),selected = getLatestCycle(input$base))
    }
  })
  # select_cycle_SA
  output$select_cycle_SA <- renderUI({
    print("DEBUG: -> select_cycle_SA")
    if ( is.null(input$base_SA)) {
      selectInput("cycle_SA",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"),selected = getLatestCycle("Screening")
      )
    }else{
      selectInput("cycle_SA",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"),selected = getLatestCycle(input$base_SA))
    }
  })

 
  # select_obtype
  output$select_obtype <- renderUI({
     if ( verbose("DEBUG") ) {print("DEBUG: -> select_obtype") }
     selectInput(inputId = "obtype",label=h5("Select observation type"),choices=getObtypes(input$ODBbase,input$dateRange,input$cycle))
  })
  # select_obtype_SA
  output$select_obtype_SA <- renderUI({
     if ( verbose("DEBUG")) { print("DEBUG: -> select_obtype_SA") }
     selectInput(inputId = "obtype_SA",label=h5("Select observation type"),choices=getObtypes(input$ODBbase_SA,input$dateRange_SA,input$cycle_SA))
  })


  # select_plottype
  output$select_plottype <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype") }
    selectInput(inputId = "plottype",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype,input$dateRange,input$cycle))
  })
  # select_plottype_SA
  output$select_plottype_SA <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype_SA") }
    selectInput(inputId = "plottype_SA",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype_SA,input$dateRange_SA,input$cycle_SA))
  })


  # select_group_predef
  output$select_group_predef <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_group_predef") }
    selectInput(inputId = "groupPreDef",label=h5("Which group?"),choices=getPreDefinedGroups())
  })

  # select_plottype_predef
  output$select_plottype_predef <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype_predef") }
    selectInput(inputId = "plottypePreDef",label=h5("Select type of plot"),choices=getPreDefinedPlots(input$groupPreDef))
  })

  # select_variable
  output$select_variable <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_variable") }
    selectInput(inputId = "variable",label=h5("Select variable"),choices=getVariables(input$obtype,input$ODBbase,input$dateRange,input$cycle))
  })
  # select_variable_SA
  output$select_variable_SA <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_variable_SA") }
    selectInput(inputId = "variable_SA",label=h5("Select variable"),choices=getVariables(input$obtype_SA,input$ODBbase_SA,input$dateRange_SA,input$cycle_SA))
  })

  # select_level
  output$select_level <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_level") }
    selectInput(inputId = "level",label=h5("Select level"),choices=getLevels(input$obtype,input$variable,getPlotTypeShort(input$plottype)),multiple=T)
  })

  # select_sensor
  output$select_sensor <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_sensor") }
    selectInput(inputId = "sensor",label=h5("Select sensor"),choices=getSensors(input$ODBbase,input$dateRange,input$cycle))
  })

  # select_satelite
  output$select_satelite <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_satelite") }
    selectInput(inputId = "satelite",label=h5("Select satelite"),choices=getSatelites(input$ODBbase,input$sensor,input$dateRange,input$cycle))
  })

  # select_channel
  output$select_channel <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_channel") }
    selectInput(inputId = "channel",label=h5("Select channel"),choices=getChannels(input$ODBbase,input$sensor,input$satelite,input$dateRange,input$cycle),multiple=T)
  })

  # select_experiment
  output$select_experiment <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_experiment") }
    if ( !is.null(getExperiments(input$ODBbase))) {
      selectInput(inputId = "experiment",label=h5("Select pre-defined experiment"),choices=getExperiments(input$ODBbase))
    } else {
      if ( !is.null(input$ODBbase) ) {
        if ( input$ODBbase == "Screening" ) {
          fileInput('ODBbase_screening', 'Choose SQLite data base from screening',accept = c('.db'))
        } else if ( input$ODBbase == "Minimization" ) {
          fileInput('ODBbase_minimization', 'Choose SQLite data base from minimization',accept = c('.db'))
        }
      }
    }
  })
  # select_experiment_SA
  output$select_experiment_SA <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_experiment_SA") }
    if ( !is.null(getExperiments(input$ODBbase_SA))) {
      selectInput(inputId = "experiment_SA",label=h5("Select pre-defined experiment"),choices=getExperiments(input$ODBbase_SA))
    } else {
      if ( !is.null(input$ODBbase_SA) ) {
        if ( input$ODBbase_SA == "Surface" ) {
          fileInput('ODBbase_surface', 'Choose SQLite data base from surface assimilation',accept = c('.db'))
        }
      }
    }
  })

  # set_verbosity
  output$set_verbosity <- renderUI({
    selectInput(inputId="verbosity_chosen",label=h4("Verbosity"),choices=c("NONE","INFO","DEBUG"),selected=c("DEBUG"))
  })

  # ObsmonPlot
  output$ObsmonPlot <- renderPlot({
    input$doPlot
    if ( verbose("DEBUG") ) {
      print("======================")
      print("> DEBUG: -> ObsmonPlot")
      print("======================")
    }
    if ( is.null(input$doPlot)) {
      return(NULL)
    }else{
      if ( as.integer(input$doPlot) == 0 ) {
        return(NULL)
      }else{
        isolate({
          switch(input$obtype, SATEM = {var = "rad"},{ var = input$variable})
          obPlot <- generatePlot(input$ODBbase,getPlotTypeShort(input$plottype),getObNumber(input$obtype),var,getUnit(var),input$obtype,input$sensor,input$satelite,input$channel,input$dateRange,input$cycle) 

          return(obPlot)
        })
      }
    }
  },height="auto",width=widthOfPlot)

  # ObsmonPlot_SA
  output$ObsmonPlot_SA <- renderPlot({
    input$doPlot_SA
    if ( verbose("DEBUG") ) {
      print("=========================")
      print("> DEBUG: -> ObsmonPlot_SA")
      print("=========================")
    }

    if ( is.null(input$doPlot_SA)) {
      return(NULL)
    }else{
      if ( as.integer(input$doPlot_SA) == 0 ) {
        return(NULL)
      }else{
        isolate({

          obPlot <- generatePlot(input$ODBbase_SA,getPlotTypeShort(input$plottype_SA),getObNumber(input$obtype_SA),input$variable_SA,getUnit(input$variable_SA),input$obtype_SA,NA,NA,"Surface",input$dateRange_SA,input$cycle_SA)

          return(obPlot)
        })
      }
    }
  },height="auto",width=widthOfPlot)

  # ObsmonPlotPreDef
  output$ObsmonPlotPreDef <- renderPlot({
    input$doPlotPreDef
    if ( verbose("DEBUG") ) {
      print("============================")
      print("> DEBUG: -> ObsmonPlotPreDef")
      print("============================")
    }
    if ( is.null(input$doPlotPreDef)) {
      return(NULL)
    }else{
      if ( as.integer(input$doPlotPreDef) == 0 ) {
        return(NULL)
      }else{
        isolate({

          obPlot<-NULL
          switch(isolate(input$plottypePreDef),
                 "Plot 1" ={ obPlot <- generatePlot(c("Surface"),getPlotTypeShort("Observation usage (map)"),getObNumber(c("SYNOP")),c("t2m"),getUnit("t2m"),c("SYNOP"),NA,NA,"Surface",c(getLatestDate("Surface"),getLatestDate("Surface")),c(getLatestCycle("Surface")))},
                 "Plot 2" ={ obPlot <- generatePlot(c("Surface"),getPlotTypeShort("Observation usage (map)"),getObNumber(c("SYNOP")),c("snow"),getUnit("snow"),c("SYNOP"),NA,NA,"Surface",c(getLatestDate("Surface"),getLatestDate("Surface")),c(getLatestCycle("Surface")))},
                 "Plot 3" ={obPlot <- generatePlot(c("Minimization"),getPlotTypeShort("Number of observations (TS)"),getObNumber(c("TEMP")),c("t"),getUnit("t"),c("TEMP"),NA,NA,"ALL",c(getLatestDate("Minimization"),getLatestDate("Minimization")),c(getLatestCycle("Minimization")))}
          )
          return(obPlot)
        })
      }
    }
  },height="auto",width=widthOfPlot) 

  #commentPreDefined
  output$commentPreDefined <- renderText({"(*) Right click to save figures as .png"})

  # query_used
  output$query_used<- renderText({
    values$plotQuery
  })
  # query_used_SA
  output$query_used_SA<- renderText({
    values$plotQuery
  })
  # query_usedPreDefined
  output$query_usedPreDefined<- renderText({
    values$plotQuery
  })
    
  # data_plotted                 
  output$data_plotted<-renderTable({
    if ( verbose("DEBUG") ) { print("DEBUG: data_plotted")}
    values$plotData
  })
  # data_plotted_SA              
  output$data_plotted_SA<-renderTable({
    if ( verbose("DEBUG") ) { print("DEBUG: data_plotted_SA")}
    values$plotData
  })
  # data_plottedPreDefined          
  output$data_plottedPreDefined<-renderTable({
    if ( verbose("DEBUG") ) { print("DEBUG: data_plottePreDefined")}
    values$plotData
  })

  # dumpDB
  output$dumpDB <- renderTable({
    if ( verbose("DEBUG") ) { print("DEBUG: dumpDB")}
    input$doDump

    isolate(
      if ( is.null(input$ODBbase_dump)) {
        return(NULL)
      } else {
        if ( as.integer(input$doDump) == 0 ) {
          return(NULL)
        }else{
          if ( !is.null(input$dump_table)) {
            getDumpData(input$ODBbase_dump,input$dump_table)
          }else{
            return(NULL)
          }
        }
      }
    )
  })
  

  # downloadImage
  output$downloadImage <- downloadHandler (
    filename = function () {
      paste(input$ODBbase,"_",input$plottype,"_",input$variable,"_",input$obtype,"_",input$sensor,"_",input$satelite,"_",input$channel,"_",input$dateRange,"_",input$cycle,".",input$plotTypeFormat,sep="")
    },
    content = function(file) {
      xWidth=10
      yHeight=5.5
      DPI <- 150

      switch(input$obtype, SATEM = {var = "rad"},{ var = input$variable})
      switch(input$plotTypeFormat,
             "eps" = {setEPS()
                      postscript(file,width=xWidth,height=yHeight)},
             "pdf" =  pdf(file,width=xWidth,height=yHeight),
             "png" =  png(file,width=xWidth*DPI,height=yHeight*DPI,res=DPI)
      )
      obPlot <- generatePlot(input$ODBbase,getPlotTypeShort(input$plottype),getObNumber(input$obtype),var,getUnit(var),input$obtype,input$sensor,input$satelite,input$channel,input$dateRange,input$cycle)
      print(obPlot)
      dev.off()
    }
  )

  # downloadImage
  output$downloadImage_SA <- downloadHandler (
    filename = function () {
      paste(input$ODBbase_SA,"_",input$plottype_SA,"_",input$variable_SA,"_",input$obtype_SA,"_",input$channel,"_",input$dateRange,"_",input$cycle,".",input$plotTypeFormat,sep="")
    },
    content = function(file) {
      xWidth=10
      yHeight=5.5
      DPI <- 150
      switch(input$plotTypeFormat_SA,
             "eps" = {setEPS()
                      postscript(file,width=xWidth,height=yHeight)},
             "pdf" =  pdf(file,width=xWidth,height=yHeight),
             "png" =  png(file,width=xWidth*DPI,height=yHeight*DPI,res=DPI)
      )
      obPlot <- generatePlot(input$ODBbase_SA,getPlotTypeShort(input$plottype_SA),getObNumber(input$obtype_SA),input$variable_SA,getUnit(input$variable_SA),input$obtype_SA,NA,NA,input$channel_SA,input$dateRange_SA,input$cycle_SA)
      print(obPlot)
      dev.off()
    }
  )


  ###############################################################

  # Dummy debug plot
  #generatePlot <- function(odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle){
  #  if ( verbose("DEBUG") ) { print(paste("DEBUG: -> generatePlot",odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle)) }
  #  p <- NULL
  #  title <- paste(odbBase,plotName,obNumber,varName,unit,obName,sensor,satelite,channel,dateRange,cycle)
  #  #print(title)
  #  p <- ggplot(mtcars, aes(wt, mpg))
  #  p <- p + geom_point()
  #  p <- p + labs(title = title)
  #  return(p)
  #}
  
})
