# Define server logic required to plot various variables

require(shiny)
require(RSQLite)
library(ggplot2)
require(jpeg)
require(reshape2)

lastPlot=0
lastPlot_SA=0
lastPlotPreDef=0
map.world<-map_data(map="world")

shinyServer(function(input,output) {

  # Source functions
  source("./obsmon_functions.R",local=TRUE)
  source("./obsmon_plots.R",local=TRUE)

  # getData (Checks for file uploading/selection)
  getData <- reactive({
    if(!is.null(input$ODBbase_screening)){
    }
    if(!is.null(input$ODBbase_minimization)){
    }
    if(!is.null(input$ODBbase_surface)){
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
  # select_obtype
  output$select_obtype <- renderUI({
     selectInput(inputId = "obtype",label=h5("Select observation type"),choices=getObtypes(input$ODBbase))
  })
  output$select_obtype_SA <- renderUI({
     selectInput(inputId = "obtype_SA",label=h5("Select observation type"),choices=getObtypes(input$ODBbase_SA))
  })


  # select_plottype
  output$select_plottype <- renderUI({
    selectInput(inputId = "plottype",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype,input$dateRange))
  })
  output$select_plottype_SA <- renderUI({
    selectInput(inputId = "plottype_SA",label=h5("Select type of plot"),choices=getPlotTypes(input$obtype_SA,input$dateRange_SA))
  })


  # select_plottype_predef
  output$select_group_predef <- renderUI({
    selectInput(inputId = "groupPreDef",label=h5("Which group?"),choices=getPreDefinedGroups())
  })

  # select_plottype_predef
  output$select_plottype_predef <- renderUI({
    selectInput(inputId = "plottypePreDef",label=h5("Select type of plot"),choices=getPreDefinedPlots(input$groupPreDef))
  })

  # select_variable
  output$select_variable <- renderUI({
    selectInput(inputId = "variable",label=h5("Select variable"),choices=getVariables(input$obtype,input$ODBbase))
  })
  output$select_variable_SA <- renderUI({
    selectInput(inputId = "variable_SA",label=h5("Select variable"),choices=getVariables(input$obtype_SA,input$ODBbase_SA))
  })

  # select_level
  output$select_level <- renderUI({
    selectInput(inputId = "level",label=h5("Select level"),choices=getLevels(input$obtype,input$variable,getPlotTypeShort(input$plottype)),multiple=T)
  })

  # select_sensor
  output$select_sensor <- renderUI({
    selectInput(inputId = "sensor",label=h5("Select sensor"),choices=getSensors())
  })

  # select_satelite
  output$select_satelite <- renderUI({
    selectInput(inputId = "satelite",label=h5("Select satelite"),choices=getSatelites(input$sensor))
  })

  # select_channel
  output$select_channel <- renderUI({
    selectInput(inputId = "channel",label=h5("Select channel"),choices=getChannels(input$sensor),multiple=T)
  })

  # select_experiment1
  output$select_experiment1 <- renderUI({
    if ( !is.null(getExperiments(input$ODBbase))) {
      selectInput(inputId = "experiment",label=h5("Select pre-defined experiment"),choices=getExperiments(input$ODBbase))
    } else {
      fileInput('ODBbase_screening', 'Choose SQLite data base from screening',accept = c('.db'))
    }
  })
  # select_experiment2
  output$select_experiment2 <- renderUI({
    if ( is.null(getExperiments(input$ODBbase))) {
      fileInput('ODBbase_minimization', 'Choose SQLite data base from minimization',accept = c('.db'))
    }
  })

  # ObsmonPlot
  output$ObsmonPlot <- renderPlot({
    if ( length(input$doPlot) > 0 ){
      if ( as.integer(input$doPlot) > as.integer(lastPlot) ) {
          isolate({
          lastPlot=as.integer(input$doPlot)

          obPlot <- generatePlot(input$ODBbase,getPlotTypeShort(input$plottype),getObNumber(input$obtype),input$variable,getUnit(input$variable),input$obtype,input$sensor,input$satelite,input$channel,input$dateRange,input$cycle) 

          return(obPlot)
        })
      }
    }
  })

  output$ObsmonPlot_SA <- renderPlot({
    if ( length(input$doPlot_SA) > 0 ){
      if ( as.integer(input$doPlot_SA) > as.integer(lastPlot_SA) ) {
          isolate({
          lastPlot_SA=as.integer(input$doPlot_SA)

          obPlot <- generatePlot(input$ODBbase_SA,getPlotTypeShort(input$plottype_SA),getObNumber(input$obtype_SA),input$variable_SA,getUnit(input$variable_SA),input$obtype_SA,NA,NA,"Surface",input$dateRange_SA,input$cycle_SA)

          return(obPlot)
        })
      }
    }
  })

  # ObsmonPlotPreDef
  output$ObsmonPlotPreDef <- renderPlot({
    if ( length(input$doPlotPreDef) > 0 ){
      if ( as.integer(input$doPlotPreDef) > as.integer(lastPlotPreDef) ) {
        isolate({
          lastPlotPreDef=as.integer(input$doPlotPreDef)
          obPlot<-NULL
          switch(input$plottypePreDef,
                 "Plot 1" ={ obPlot <- generatePlot(c("Surface"),getPlotTypeShort("Observation usage (map)"),getObNumber(c("SYNOP")),c("t2m"),getUnit("t2m"),c("SYNOP"),NA,NA,"Surface",c("2014-06-06","2014-06-06"),c("00"))}
          )
          return(obPlot)
        })
      }
    }
  }) 

  ###############################################################
  
  ########################### Buttons ###########################
  output$plotButton <- renderUI({
    actionButton("doPlot", label = "Plot!")
  })
  output$plotButton_SA <- renderUI({
    actionButton("doPlot_SA", label = "Plot!")
  })

  output$plotButtonPreDef <- renderUI({
    actionButton("doPlotPreDef", label = "Generate pre-defined plot!")
  })
  ################################################################

})
