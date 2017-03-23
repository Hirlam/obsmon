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
        tagList(
            tags$head(tags$title("Obsmon v2")),
            h3("Obsmon v2", style="text-align: center;"),
            hr()
        ),
        sidebarLayout(
            sidebarPanel(
                width=3,
                selectInput("experiment",
                            label="Experiment",
                            choices=c()),
                selectInput("category",
                            label=NULL,
                            choices=list("Upper Air (3D-VAR/4D-VAR)"="upperAir",
                                         "Surface (CANARI)"="surface")),
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
                                choices=c()),
                    fluidRow(
                        column(8,
                               dateRangeInput("dateRange",
                                              label="Date Range"),
                               shinyjs::hidden(
                                            dateInput("date", "Date")
                                        )),
                        column(4,
                               selectInput("cycle",
                                           label="Cycle",
                                           choices=c()))
                    )
                ),
                actionButton("doPlot", "Plot", width="100%")
            ),
            mainPanel(
                width=9,
                tabsetPanel(
                    tabPanel(
                        "Plot",
                        fluidRow(
                            column(12, align="center",
                                   tags$head(tags$style("#plot{height:80vh !important;}")),
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
