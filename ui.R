jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

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
        extendShinyjs(text=jscode),
        inlineCSS(css),
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
                            label="Category",
                            choices=list("Upper Air (3D-VAR/4D-VAR)"="upperAir",
                                         "Surface (CANARI)"="surface")),
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
                                selectize=FALSE),
                    selectInput("station",
                                "Station",
                                choices=c())
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
                ),
                actionButton("doPlot", "Plot", width="100%")
            ),
            mainPanel(
                width=9,
                tabsetPanel(
                    id="mainArea",
                    tabPanel(
                        "Plot",
                        value="plotTab",
                        fluidRow(
                            column(12, align="center",
                                   tags$head(tags$style("#plot{height:80vh !important;}")),
                                   plotOutput(outputId="plot", height="auto", width="auto"),
                                   tags$style(type="text/css", "body { overflow-y: scroll; }")
                                   )
                        )
                    ),
                    tabPanel(
                        "Map",
                        value="mapTab",
                        fluidRow(
                            column(12,
                                   textOutput("mapTitle"),
                                   tags$style(type="text/css", ".shiny-text-output { text-align: center; }")
                                   )
                        ),
                        fluidRow(
                            column(12, align="center",
                                   tags$head(tags$style("#map{height:80vh !important;}")),
                                   leafletOutput(outputId="map", height="auto", width="auto"),
                                   tags$style(type="text/css", "body { overflow-y: scroll; }")
                                   )
                        )
                    ),
                    tabPanel(
                        value="dataTab",
                        "Query and data",
                        wellPanel(h5("Query used:"),
                                  textOutput("queryUsed")
                                  ),
                        h5("Data:"),
                        dataTableOutput("dataTable")
                    )
                )
            )
        )
    )
)
