# Define server logic required to plot various variables

require(shiny)
require(RSQLite)
require(ggplot2)
require(jpeg)
require(reshape2)
require(chron)
require(scales)
require(mapproj)

widthOfPlot=800
map.world<-map_data(map="world")

values<-reactiveValues()
values$plotData=NULL
values$plotQuery=NULL
values$productionSite=FALSE
values$last_obtype=NULL
values$last_base=NULL
values$last_variable=NULL
values$last_level=NULL
values$last_plot=NULL
values$last_sensor=NULL
values$last_satelite=NULL
values$last_channel=NULL
values$synops=NULL

shinyServer(function(input,output) {

  maxUpload<-observe({
    # Set upload file size
    if ( is.null(input$maxUpload)){
      options(shiny.maxRequestSize=30*1024^2)
    } else{
      options(shiny.maxRequestSize=input$maxUpload*1024^2)
    }
  })

  # Source functions
  source("./obsmon_functions.R",local=TRUE)
  source("./obsmon_predefined.R",local=TRUE)
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

  # select_base 
  output$select_base<- renderUI({
    selectInput("ODBbase",h5("Monitoring level:"),c("Screening","Minimization"),selected=getLastSelected("last_base"))
  })

  # select_dump_base 
  output$select_dump_base<- renderUI({
     selectInput("ODBbase_dump",h5("Monitoring level to dump:"),c("Screening","Minimization","Surface"),selected=getLastSelected("last_base"))
  })

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
      dateRangeInput("dateRange_SA",
        label = h5("Date range"),
        start = getLatestDate("Surface"),
        end   = getLatestDate("Surface")
    )
  })
  # select_cycle
  output$select_cycle <- renderUI({
    if ( verbose("DEBUG")) { print("DEBUG: -> select_cycle") }
    if ( is.null(input$base)) {
      selectInput("cycle",h5("Cycle"),c("00","03","06","09","12","15","18","21"),selected = getLatestCycle("Screening")
      )
    }else{
      selectInput("cycle",h5("Cycle"),c("00","03","06","09","12","15","18","21"),selected = getLatestCycle(input$base))
    }
  })
  # select_cycle_SA
  output$select_cycle_SA <- renderUI({
    print("DEBUG: -> select_cycle_SA")
    if ( is.null(input$base_SA)) {
      selectInput("cycle_SA",h5("Cycle"),c("00","03","06","09","12","15","18","21"),selected = getLatestCycle("Screening")
      )
    }else{
      selectInput("cycle_SA",h5("Cycle"),c("00","03","06","09","12","15","18","21"),selected = getLatestCycle(input$base_SA))
    }
  })

 
  # select_obtype
  output$select_obtype <- renderUI({
     if ( verbose("DEBUG") ) {print("DEBUG: -> select_obtype") }
     selectInput(inputId = "obtype",label=h5("Select observation type"),choices=getObtypes(),selected=getLastSelected("last_obtype")) 
  })
  # select_obtype_SA
  output$select_obtype_SA <- renderUI({
     if ( verbose("DEBUG")) { print("DEBUG: -> select_obtype_SA") }
     selectInput(inputId = "obtype_SA",label=h5("Select observation type"),choices=getObtypes(),selected=getLastSelected("last_obtype"))
  })


  # select_plottype
  output$select_plottype <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype") }
    selectInput(inputId = "plottype",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype),selected=getLastSelected("last_plot"))
  })
  # select_plottype_SA
  output$select_plottype_SA <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype_SA") }
    selectInput(inputId = "plottype_SA",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype_SA),selected=getLastSelected("last_plot"))
  })


  # select_group_predef
  output$select_group_predef <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_group_predef") }
    selectInput(inputId = "groupPreDef",label=h5("Which group?"),choices=getPreDefinedGroups())
  })

  # select_plottype_predef
  output$select_plottype_predef <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_plottype_predef") }
    selectInput(inputId = "plottypePreDef",label=h5("Select type of plot"),choices=getPreDefinedPlots(input$groupPreDef),width="100%",selected=getLastSelected("last_plot"))
  })

  # select_variable
  output$select_variable <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_variable") }
    selectInput(inputId = "variable",label=h5("Select variable"),choices=getVariables(input$obtype),selected=getLastSelected("last_variable"))
  })
  # select_variable_SA
  output$select_variable_SA <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_variable_SA") }
    selectInput(inputId = "variable_SA",label=h5("Select variable"),choices=getVariables(input$obtype_SA),selected=getLastSelected("last_variable"))
  })

  # select_level
  output$select_level <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_level") }
    selectInput(inputId = "level",label=h5("Select level"),choices=getLevels(input$obtype,input$variable,getPlotTypeShort(input$plottype)),multiple=T,selected=getLastSelected("last_level"))
  })

  # select_sensor
  output$select_sensor <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_sensor") }
    selectInput(inputId = "sensor",label=h5("Select sensor"),choices=getSensors(),selected=getLastSelected("last_sensor"))
  })

  # select_satelite
  output$select_satelite <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_satelite") }
    selectInput(inputId = "satelite",label=h5("Select satelite"),choices=getSatelites(input$sensor),selected=getLastSelected("last_satelite"))
  })

  # select_channel
  output$select_channel <- renderUI({
    if ( verbose("DEBUG") ) { print("DEBUG: -> select_channel") }
    selectInput(inputId = "channel",label=h5("Select channel"),choices=getChannels(input$sensor,input$satelite),multiple=T,selected=getLastSelected("last_channel"))
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
    if ( !is.null(getExperiments())) {
      selectInput(inputId = "experiment_SA",label=h5("Select pre-defined experiment"),choices=getExperiments())
    } else {
      if ( !is.null(input$tabs) ) {
        if ( input$tabs == "Surface" ) {
          fileInput('ODBbase_surface', 'Choose SQLite data base from surface assimilation',accept = c('.db'))
        }
      }
    }
  })

  # set_verbosity
  output$set_verbosity <- renderUI({
    if ( values$productionSite ) {
      selectInput(inputId="verbosity_chosen",label=h4("Verbosity"),choices=c("NONE"),selected=c("NONE"))
    }else{
      selectInput(inputId="verbosity_chosen",label=h4("Verbosity"),choices=c("NONE","WARNING","INFO","DEBUG"),selected=c("DEBUG"))
    }
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
          values$last_plot=input$plottype
          switch(input$obtype, SATEM = {var = "rad"},{ var = input$variable})
          obPlot <- generatePlot(input$ODBbase,getPlotTypeShort(input$plottype),input$obtype,var,input$level,input$sensor,input$satelite,input$channel,input$dateRange,input$cycle) 

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

          values$last_plot=input$plottype_SA
          obPlot <- generatePlot("Surface",getPlotTypeShort(input$plottype_SA),input$obtype_SA,input$variable_SA,"Surface",NULL,NULL,NULL,input$dateRange_SA,input$cycle_SA)

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

          values$last_plot=input$plottypePreDef
          obPlot=generatePreDefinedPlot(input$plottypePreDef)
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

  #dumpDB_button
  output$dumpDB_button<-renderUI({
    if ( values$productionSite ){
      actionButton("doDumpDisabled", label = "Disabled for productions sites")
    }else{
      actionButton("doDump", label = "Dump database")
    }
  })

  # dumpDB
  output$dumpDB <- renderTable({
    if ( verbose("DEBUG") ) { print("DEBUG: dumpDB")}
    input$doDump

    isolate(
      if ( is.null(input$ODBbase_dump)) {
        return(NULL)
      } else {
        if ( !is.null(input$doDump)) {
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
      obPlot <- generatePlot(input$ODBbase,getPlotTypeShort(input$plottype),input$obtype,var,input$level,input$sensor,input$satelite,input$channel,input$dateRange,input$cycle)
      print(obPlot)
      dev.off()
    }
  )

  # downloadImage
  output$downloadImage_SA <- downloadHandler (
    filename = function () {
      paste("Surface_",input$plottype_SA,"_",input$obtype_SA,"_",input$variable_SA,"_Surface_",input$dateRange,"_",input$cycle,".",input$plotTypeFormat,sep="")
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
      obPlot <- generatePlot("Surface",getPlotTypeShort(input$plottype_SA),input$obtype_SA,input$variable_SA,"Surface",NULL,NULL,NULL,input$dateRange_SA,input$cycle_SA)
      print(obPlot)
      dev.off()
    }
  )

  output$select_stations_surfdia<-renderUI({
    selectInput(inputId = "station",label=h5("Select station:"),choices=getStations(input$variable_surfdia),width="100%")
  })
  
  # ObsmonPlotPreDef
  output$surfdiaPlot <- renderPlot({
    input$doPlotSurfdia
    if ( verbose("DEBUG") ) {
      print("============================")
      print("> DEBUG: -> surfdiaPlot     ")
      print("============================")
    }
    if ( is.null(input$doPlotSurfdia)) {
      return(NULL)
    }else{
      if ( as.integer(input$doPlotSurfdia) == 0 ) {
        return(NULL)
      }else{
        isolate({

          obPlot=NULL
          obPlot=generate_surfdia(input$variable_surfdia,input$station)
          return(obPlot)
        })
      }
    }
  },height="auto",width=widthOfPlot)

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
