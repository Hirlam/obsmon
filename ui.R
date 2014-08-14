library(shiny)

shinyUI(navbarPage("OBSMON v2",id="tabs",
  tabPanel("Upper air (3D-VAR/4D-VAR)",
    fluidRow(
      column(3,
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
      column(9,
        fluidRow(
          column(2,
             uiOutput("select_base")
          ),
          column(3,
            uiOutput("select_obtype")
          ),
          column(5,
            uiOutput("select_plottype"),
            tags$style(type='text/css', "#select_plottype { width: 250px;}"),

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
  tabPanel("Surface (CANARI)",value="Surface",
    fluidRow(
      column(3,
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
          uiOutput("select_experiment_SA")
        )
      ),  
      column(9,
        fluidRow(
          column(4,
            fluidRow(
              column(12,
                uiOutput("select_obtype_SA")
              )   
            )  
          ),
          column(4,
            uiOutput("select_plottype_SA"),
            tags$style(type='text/css', "#select_plottype_SA { width: 250px;}"),  
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
      column(3,align="right",
        uiOutput("select_group_predef")
      ),
      column(6,
        uiOutput("select_plottype_predef"),
        tags$style(type='text/css', "#select_plottype_predef { width: 100%;}")
      ),
      column(3,
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
  tabPanel("Surface diagnostics",value="surfdia",
    fluidRow(
      column(4,align="right",
        uiOutput("select_variable_surfdia")
      ),
      column(4,
        uiOutput("select_stations_surfdia"),
        tags$style(type='text/css', "#select_stations_surfdia { width: 100%;}")
      ),
      column(4,
         actionButton("doPlotSurfdia", label = "Plot!"),
         tags$style(type='text/css', "#doPlotSurfdia { vertical-align: middle; height: 70px; width: 200px; background: green; color: white;}")
      )
    ),
    hr(),
    fluidRow(
      column(10,offset=2,
        fluidRow(
          column(12,
            plotOutput(outputId="surfdiaPlot"),
            tags$style(type="text/css", "body { overflow-y: scroll; }")
          )
        )
      )
    )
  ),
  tabPanel("Dump database",
    fluidRow(
      column(12,align="center",
        wellPanel(
          h5("NB! Do not dump big data bases as this could be demanding for your system")
        )
      )
    ),
    fluidRow(
      column(4,align="right",
             uiOutput("select_dump_base")
      ),
      column(4,align="right",
             selectInput("dump_table",h5("Table to dump:"),c("obsmon","usage"))
      ),
      column(4,
         uiOutput("dumpDB_button"),
         tags$style(type='text/css', "#doDump { vertical-align: middle; height: 70px; width: 200px; background: green; color: white;}"),
         tags$style(type='text/css', "#doDumpDisabled { vertical-align: middle; height: 70px; width: 200px; background: red; color: white;}")
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
          checkboxInput(inputId="showExistingDataOnly",label=h5("Show existing data only"),value=TRUE),
          numericInput(inputId="maxUpload",label=h5("Max file size for SQLite data bases (MB)"),value=30,min = 0)
        )
      )
    )
  ),
  tabPanel("Help",
    fluidRow(
      column(12,
        wellPanel(h2("About"),
          p("Developed by ",a("HIRLAM",href="http://hirlam.org",style = "color:blue")," as a tool to monitor observation usage in ",
          span("HARMONIE",style = "color:blue"),".")
        )
      ),
      wellPanel(h2("Help"),
        h4("Usage"),
        p("Shiny is reactive. It means when you change something that has a dependency, the dependency will adjust."),
        p("There is one tab for upper air assimilation and one for surface assimilation. The menus have a dependency from left to right, and you will need to press the button for plotting to get the current plot. Next to the plot you can see the SQL query and extracted data for the last plot excecuted."), 
        h5("Pre-defined plots"),
        p("This tab is meant as a short-cut for frequently used plots. It is designed for several groups so that e.g. duty forecasters can monitor the production by using their own group of pre-defined plots."),
        h5("Settings"),
        span("Show existing data only",style="color:red"),
        p("By enabling this, the user experience will be slower but you will only show options found in the selected SQLite table. The menus have a dependency from left to right. It means if you change the timing, most of the other options will have a dependency and will be re-checked. There has been implemented a solution that will remember the last values when plotting is executed. It means you will start on the last plotted values as your current settings, if you plot a figure and then adjust the time period. If you disable the check for exixting data the options will have a normal dependency from left to right."),
        p("In the scripts there is a setting productionSite which should be set TRUE for sites running the OBSMON shiny interface as a constantly running daemon. This switch disables potentially performance destroying features like the dumping of the SQLite data base"),
        h5("Dump database"),
        p("This tab is good for debugging of small data bases, but should not be used on large operational data bases as it could overload your system.") 
      )
    )
  )
))
