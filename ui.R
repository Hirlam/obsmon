library(shiny)

shinyUI(navbarPage("OBSMON v2",
  tabPanel("Upper air (3D-VAR/4D-VAR)",
    fluidRow(
      column(2,
        wellPanel(
          fluidRow(
            column(7,
              uiOutput("select_date")
            ),
            column(4,offset=1,
              uiOutput("select_cycle")
            )
          ),
          hr(),
          uiOutput("select_experiment")
        )
      ),
      column(10,
        fluidRow(
          column(5,
            fluidRow(
              column(6,
                uiOutput("select_obtype")
              )
            ),
            fluidRow(
              column(6,
                uiOutput("select_plottype")
              )
            )
          ),
          column(3,
            # Only show this panel if obtype == SATEM
            conditionalPanel(
              condition = "input.obtype == 'SATEM'",
              uiOutput("select_sensor"),
              uiOutput("select_satelite"),
              uiOutput("select_channel")

            ),
            conditionalPanel(
              condition = "input.obtype != 'SATEM'",
              uiOutput("select_variable"),
              uiOutput("select_level")
            )
          ),
          column(2,
             selectInput("ODBbase",h5("Monitoring level:"),c("Screening","Minimization"))
          ),
          column(2,
            actionButton("doPlot", label = "Plot!"),
            tags$style(type='text/css', "#doPlot { vertical-align: middle; height: 70px; width: 100px; background: green; color: white;}")
          )
        ),
        hr(),
        fluidRow(
          column(12,
            tabsetPanel(
              tabPanel("Plot",
                fluidRow(
                  column(10,
                    plotOutput(outputId="ObsmonPlot"),
                    tags$style(type="text/css", "body { overflow-y: scroll; }")
                  ),
                  column(2,
                    conditionalPanel(
                      condition = "input.doPlot != 0",
                      wellPanel(
                        radioButtons('plotTypeFormat','Format of plot to download',c('eps','pdf','png'),'eps'),
                        downloadButton("downloadImage","Download Plot")
                      )
                    )
                  )
                )
              ),
              tabPanel("Query and data used in last plot",
                wellPanel(h5("Query used:"),
                  uiOutput("query_used")
                ),
                hr(),
                br(),
                wellPanel(h5("Data:"),
                  uiOutput("data_plotted")
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel("Surface (CANARI)",
    fluidRow(
      column(2,
        wellPanel(
          fluidRow(
            column(7,
              uiOutput("select_date_SA")
            ),  
            column(4,offset=1,
              uiOutput("select_cycle_SA")
            )   
          ),  
          hr(),
          uiOutput("select_experiment_SA"),
          selectInput("ODBbase_SA",h5("Monitoring level:"),c("Surface")),
          selectInput("level_SA",label=h5("Select level"),choices=c("Surface"))
        )
      ),  
      column(10,
        fluidRow(
          column(4,
            fluidRow(
              column(6,
                uiOutput("select_obtype_SA")
              )   
            ),  
            fluidRow(
              column(6,
                uiOutput("select_plottype_SA")
              )   
            )   
          ),  
          column(4,
            uiOutput("select_variable_SA")
          ),  
          column(4,
            uiOutput("plotButton_SA"),
            actionButton("doPlot_SA", label = "Plot!"),
            tags$style(type='text/css', "#doPlot_SA { vertical-align: middle; height: 70px; width: 100px; background: green; color: white;}")
          )   
        ),
        hr(),
        fluidRow(
          column(12,
            tabsetPanel(
              tabPanel("Plot",
                fluidRow(
                  column(10,
                    plotOutput(outputId="ObsmonPlot_SA"),
                    tags$style(type="text/css", "body { overflow-y: scroll; }")
                  ),
                  column(2,
                    conditionalPanel(
                      condition = "input.doPlot_SA != 0",

                      wellPanel(
                        radioButtons('plotTypeFormat_SA','Format of plot to download',c('eps','pdf','png'),'eps'),
                        downloadButton("downloadImage_SA","Download Plot")
                      )
                    )         
                  )
                )
              ),
              tabPanel("Query and data used in last plot",
                wellPanel(h5("Query used:"),
                  uiOutput("query_used_SA")
                ),
                hr(),
                br(),
                wellPanel(h5("Data"),
                  uiOutput("data_plotted_SA")
                )
              )
            )
          )
        )
      )
    )
  ),
  tabPanel("Pre-defined plots",
    fluidRow(
      column(4,align="right",
        uiOutput("select_group_predef")
      ),
      column(4,
        uiOutput("select_plottype_predef")
      ),
      column(4,
         actionButton("doPlotPreDef", label = "Generate pre-defined plot!"),
         tags$style(type='text/css', "#doPlotPreDef { vertical-align: middle; height: 70px; width: 200px; background: green; color: white;}")
      )
    ),
    hr(),
    fluidRow(
      column(10,offset=2,
        tabsetPanel(
          tabPanel("Plot",
            fluidRow(
              column(12,
                uiOutput("commentPreDefined")
              )
            ),
            fluidRow(
              column(12,
                plotOutput(outputId="ObsmonPlotPreDef"),
                tags$style(type="text/css", "body { overflow-y: scroll; }")
              )
            )
          ),
          tabPanel("Query and data used in last plot",
            wellPanel(h5("Query used:"),
              uiOutput("query_usedPreDefined")
            ),
            hr(),
            br(),
            wellPanel(h5("Data:"),
              uiOutput("data_plottedPreDefined")
            )
          )
        )
      )
    )
  ),
  tabPanel("Dump database",
    fluidRow(
      column(4,align="right",
             selectInput("ODBbase_dump",h5("Monitoring level to dump: (NB! Do not dump big data bases as this could be demanding for your system)"),c("Screening","Minimization","Surface"))
      ),
      column(4,align="right",
             selectInput("dump_table",h5("Table to dump:"),c("obsmon","usage"))
      ),
      column(4,
         actionButton("doDump", label = "Dump database"),
         tags$style(type='text/css', "#doDump { vertical-align: middle; height: 70px; width: 200px; background: green; color: white;}")
      )
    ),
    fluidRow(
      column(12,align="right",
        uiOutput("dumpDB")
      )
    )
  ),
  tabPanel("Settings",
    fluidRow(
      column(12,
        wellPanel(h2("Settings"),
          uiOutput("set_verbosity"),
          checkboxInput(inputId="showExistingDataOnly",label=h5("Show existing data only"),value=TRUE)
        )
      )
    )
  )
))
