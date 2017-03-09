library(shiny)
library(shinyjs)
library(leaflet)

shinyUI(
    fluidPage(
        tags$head(tags$script(HTML("
          (function() {
            $.fn.bsDatepicker.defaults.autoclose = true;
          })();
        "))),
        useShinyjs(),
        titlePanel("OBSMON v2"),
        sidebarLayout(
            sidebarPanel(
                width=3,
                selectInput("experiment",
                            label="Experiment",
                            choices=c()),
                fluidRow(
                    column(8,
                           dateRangeInput("dateRange",
                                          label="Date Range")),
                    column(4,
                           selectInput("cycle",
                                       label="Cycle",
                                       choices=c()))
                ),
                selectInput("category",
                            label=NULL,
                            choices=list("Upper Air (3D-VAR/4D-VAR)"="upperAir",
                                         "Surface (CANARI)"="surface",
                                         "Surface Diagnostics"="surfaceDiagnostics")),
                hr(),
                conditionalPanel(
                    condition=paste("input.category == 'upperAir'",
                                    "input.category == 'surface'",
                                    sep="||"),
                    selectInput("odbBase",
                                "Database",
                                choices=c()),
                    selectInput("obtype",
                                "Observation Type",
                                choices=c()),
                    conditionalPanel(
                        condition = "input.obtype == 'satem'",
                        selectInput("sensor",
                                    "Sensor",
                                    choices=c()),
                        selectInput("satellite",
                                    "Satellite",
                                    choices=c()),
                        selectInput("channels",
                                    "Channels",
                                    choices=c(),
                                    multiple=TRUE,
                                    selectize=FALSE)
                    ),
                    conditionalPanel(
                        condition = "input.obtype != 'satem'",
                        selectInput("variable",
                                    "Variable",
                                    choices=c()),
                        selectInput("levels",
                                    "Levels",
                                    choices=c(),
                                    multiple=TRUE,
                                    selectize=FALSE)
                    ),
                    selectInput("plottype",
                                "Type of Plot",
                                choices=c())
                ),
                conditionalPanel(
                    condition="input.category == 'surfaceDiagnostics'",
                    selectInput("variable_sd",
                                "Variable",
                                choices=c("T2M",
                                          "RH2M",
                                          "Snow",
                                          "U10M",
                                          "V10M",
                                          "APD",
                                          "Z"))
                ),
                actionButton("doPlot", "Plot", width="100%")
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Plot",
                        fluidRow(
                            column(10,
                                   plotOutput(outputId="plot", height="auto", width="auto"),
                                   tags$style(type="text/css", "body { overflow-y: scroll; }")
                                   )
                             )
                    )
                )
            )
        )
    )
)
