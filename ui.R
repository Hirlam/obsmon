library(shiny)

x1<-0
x2<-32
y1<-52
y2<-73

shinyUI(navbarPage("OBSMON v2",
  tabPanel("Upper air (3D-VAR/4D-VAR)",
    fluidRow(
      column(2,
        wellPanel(
          fluidRow(
            column(7,
              dateRangeInput("dateRange",
                label = h5("Date range")
              )
            ),
            column(4,offset=1,
              selectInput("cycle",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"))
            )
          ),
          hr(),
          uiOutput("select_experiment1"),
          uiOutput("select_experiment2")
        )
      ),
      column(10,
        fluidRow(
          column(3,
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
          column(3,
             selectInput("ODBbase",h5("Monitoring level:"),c("Screening","Minimization"))
          ),
          column(3,
            uiOutput("plotButton"),
            tags$style(type='text/css', "#doPlot { vertical-align: middle; height: 70px; width: 100px; background: green; color: white;}")
          )
        ),
        hr(),
        fluidRow(
          column(10,
            plotOutput(outputId="ObsmonPlot")
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
              dateRangeInput("dateRange_SA",
                label = h5("Date range")
              )   
            ),  
            column(4,offset=1,
              selectInput("cycle_SA",h5("Cycle"),c("00","03","06","09","12","15","18","21","All"))
            )   
          ),  
          hr(),
          uiOutput("select_experiment1_SA"),
          uiOutput("select_experiment2_SA"),
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
            tags$style(type='text/css', "#doPlot_SA { vertical-align: middle; height: 70px; width: 100px; background: green; color: white;}")
          )   
        ),
        hr(),
        fluidRow(
          column(10,
            plotOutput(outputId="ObsmonPlot_SA")
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
        uiOutput("plotButtonPreDef"),
            tags$style(type='text/css', "#doPlotPreDef { vertical-align: middle; height: 70px; width: 200px; background: green; color: white;}")
      )
    ),
    hr(),
    fluidRow(
      column(10,offset=2,
        plotOutput(outputId="ObsmonPlotPreDef")
      )
    )
  ),
  tabPanel("Settings",
    fluidRow(
      column(12,
        wellPanel(h4("Settings"),
          sliderInput("x1",label=h4("x1"),value=x1,min=-180,max=180),
          sliderInput("x2",label=h4("x2"),value=x2,min=-180,max=180),
          sliderInput("y1",label=h4("y1"),value=y1,min=-90,max=90),
          sliderInput("y2",label=h4("y2"),value=y2,min=-90,max=90),
          renderPrint(DBPATH_ECMA)
        )
      )
    )
  )
))
