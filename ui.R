if(!exists("initFileSourced")) source("init.R")

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
}

#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

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
        div(
            id="loading-content",
            h2(sprintf("Loading Obsmon v%s...", obsmonVersion))
        ),
        hidden(div(id="app-content",
        tagList(
            tags$head(tags$title(sprintf("Obsmon v%s", obsmonVersion))),
            h3(sprintf("Obsmon v%s", obsmonVersion),
               style="text-align: center;"),
            hr()
        ),
        sidebarLayout(
            sidebarPanel(
                width=3,
                selectizeInput("experiment",
                            multiple=FALSE,
                            "Experiment",
                            options=list(placeholder='Loading experiments...'),
                            choices=c()),
                selectizeInput("odbBase",
                            label="Data Assimilation Category/Database",
                            multiple=FALSE,
                            options=list(placeholder='Awaiting experiment initialisation...'),
                            choices=c()),
                conditionalPanel(
                    condition = "input.odbBase!='ecma_sfc'",
                    selectInput("obtype",
                                "Observation Type",
                                choices=c())
                ),
                conditionalPanel(
                    condition = "input.obtype!='satem' &&
                                 input.obtype!='scatt' &&
                                 input.obtype!='radar'",
                    selectInput("obname",
                                "Observation Name",
                                choices=c())
                ),
                conditionalPanel(
                    condition = "input.obtype == 'satem'",
                    selectInput("sensor",
                                "Sensor",
                                choices=c()),
                    selectInput("satellite",
                                "Satellite",
                                choices=c()),
                    selectInput("channels",
                                tags$div("Channels",
                                         "(Select",
                                         actionLink("channelsSelectAll", "all"),
                                         actionLink("channelsSelectNone", "none"),
                                         ")"
                                         ),
                                choices=c(),
                                multiple=TRUE,
                                selectize=FALSE)
                ),
                conditionalPanel(
                    condition = "input.obtype != 'satem'",
                    selectInput("variable",
                                "Variable",
                                choices=c()),
                    conditionalPanel(
                        condition = "input.odbBase != 'ecma_sfc' &&
                                     input.obtype!='scatt' &&
                                     input.obtype!='surface'",
                        selectInput("levels",
                                    tags$div("Levels",
                                             "(Select",
                                             actionLink("levelsSelectStandard", "standard"),
                                             actionLink("levelsSelectAll", "all"),
                                             actionLink("levelsSelectNone", "none"),
                                             ")"
                                             ),
                                    choices=c(),
                                    multiple=TRUE,
                                    selectize=FALSE)
                    )
                ),
                selectInput("plottype",
                            "Type of Plot",
                            choices=c()),
                conditionalPanel(
                    condition = "input.obtype != 'satem'",
                    selectInput("station",
                                "Station",
                                choices=c(),
                                multiple=TRUE,
                                selectize=FALSE)
                ),
                conditionalPanel(
                    condition = "output.dateType == 'single'",
                    fluidRow(
                        column(8, dateInput("date", "Date")),
                        column(4,
                          selectInput("cycle", label="Cycle", choices=c())
                        )
                    )
                ),
                conditionalPanel(
                    condition = "output.dateType == 'range'",
                    dateRangeInput(
                      "dateRange", "Date Range"
                    ),
                    checkboxGroupInput("cycles",
                                       label=tags$div("Cycles",
                                                      "(Select",
                                                      actionLink("cyclesSelectAll", "all"),
                                                      actionLink("cyclesSelectNone", "none"),
                                                      ")"
                                                      ),
                                       inline=TRUE,
                                       choices=c())
                ),
                fluidRow(
                  column(12,
                    tags$b("Cache-related actions for selected DTG(s)"),
                      HTML('&emsp;'), shiny::icon("info")
                      %>%
                      bs_embed_tooltip(
                        title = paste('Use these buttons if',
                          '"cache info not available" is shown for more than',
                          '10 seconds in any of the menus above'),
                        placement="right"
                      ),
                    br(),
                    tags$div(
                      actionButton("reloadCacheButton", "Reload cache info",
                        icon("repeat", lib="glyphicon")
                      ) %>%
                        bs_embed_tooltip(
                          title = "Reload info available in the cache file",
                          placement="above"
                        ),
                      actionButton("recacheCacheButton", "Update cache file",
                        icon("refresh", lib="glyphicon")
                      ) %>%
                        bs_embed_tooltip(
                          title = paste("Refresh cache information available",
                            "for the selected DTG(s)"),
                          placement="above"
                        )
                    ),
                    br()
                  )
                ),
                actionButton("doPlot", "Plot", width="100%",
                  icon("ok", lib="glyphicon")
                )
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
        )))
    )
)
