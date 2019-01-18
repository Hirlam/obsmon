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

cacheButtonsGeneralTooltipMsg <- paste0(
  "Use the buttons below only if you really need to make changes to the ",
  "information stored in the cache files.\n",
  "\n",
  "NB.: You can produce plots even if cache information is incomplete or ",
  "unavailable. Obsmon uses caching only to populate the GUI choices with ",
  "values that are present in the actual data files corresponding to the ",
  "selected experiment/database/DTG(s) combination."
)
recacheCacheButtonTooltipMsg <- paste0(
  "Refresh cache information available for the selected ",
  "experiment/database/DTG(s) combination.\n",
  "\n",
  "Usage case:\n",
  "  * The corresponding cached information becomes out-of-date"
)
resetCacheButtonTooltipMsg <- paste0(
  "Erase cache files belonging to the selected experiment and restart ",
  "caching from scratch.\n",
  "\n",
  "The (rare) usage cases include situations when:\n",
  "  * The cached information becomes completely out-of-date\n",
  "  * The cache files become inaccessible (e.g., corrupted)\n",
  "\n",
  "NB.: This action cannot be undone!"
)

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
                            label=getDefLabel("experiment"),
                            options=list(placeholder='Loading experiments...'),
                            choices=c()),
                selectizeInput("odbBase",
                            label=getDefLabel("odbBase"),
                            options=list(placeholder='Awaiting experiment initialisation...'),
                            multiple=FALSE,
                            choices=c()),
                conditionalPanel(
                    condition = "input.odbBase!='ecma_sfc'",
                    selectInput("obtype",
                                label=getDefLabel("obtype"),
                                choices=c())
                ),
                conditionalPanel(
                    condition = "input.obtype!='satem' &&
                                 input.obtype!='scatt' &&
                                 input.obtype!='radar'",
                    selectInput("obname",
                                label=getDefLabel("obname"),
                                choices=c())
                ),
                conditionalPanel(
                    condition = "input.obtype == 'satem'",
                    selectInput("sensor",
                                label=getDefLabel("sensor"),
                                choices=c()),
                    selectInput("satellite",
                                label=getDefLabel("satellite"),
                                choices=c()),
                    selectInput("channels",
                                tags$div(getDefLabel("channels"),
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
                                label=getDefLabel("variable"),
                                choices=c()),
                    conditionalPanel(
                        condition = "input.odbBase != 'ecma_sfc' &&
                                     input.obtype!='scatt' &&
                                     input.obtype!='surface'",
                        selectInput("levels",
                                    tags$div(getDefLabel("levels"),
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
                            label=getDefLabel("plottype"),
                            choices=c()),
                conditionalPanel(
                    condition = "input.obtype != 'satem'",
                    selectInput("station",
                                label=getDefLabel("station"),
                                choices=c(),
                                multiple=TRUE,
                                selectize=FALSE)
                ),
                fluidRow(
                    column(8, dateInput("date", "Date")),
                    column(4,
                      selectInput("cycle", label="Cycle", choices=c())
                    )
                ),
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
                                   choices=c()
                ),
                conditionalPanel(
                  condition = 'output[["showCacheOptions"]]=="TRUE"',
                  fluidRow(
                    column(12,
                      shiny::icon("exclamation-triangle"),
                      tags$b("Advanced cache-related actions"),
                        HTML('&emsp;'), shiny::icon("info")
                        %>%
                        bs_embed_tooltip(
                          title=cacheButtonsGeneralTooltipMsg,
                          placement="right"
                        ),
                      br(),
                      tags$div(
                        actionButton("recacheCacheButton",
                          "Recache DTG(s)",
                          icon("refresh", lib="glyphicon")
                        ) %>%
                          bs_embed_tooltip(
                            title=recacheCacheButtonTooltipMsg,
                            placement="above"
                          ),
                        actionButton("resetCacheButton", "Reset cache files",
                          icon("remove", lib="glyphicon"),
                          style="color: #fff; background-color: #FF0000; border-color: #2e6da4"
                        ) %>%
                          bs_embed_tooltip(
                            title=resetCacheButtonTooltipMsg,
                            placement="above"
                          )
                      ),
                      br()
                    )
                  )
                ),
                actionButton("doPlot", "Plot", width="100%",
                  icon("ok", lib="glyphicon")
                ),
                shinyjs::hidden(actionButton("cancelPlot", "Cancel plot", width="100%",
                  icon("remove", lib="glyphicon"),
                  style="color: #fff; background-color: #FFA500; border-color: #2e6da4"
                ))
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
                                   plotOutput(outputId="plot") %>% withSpinner(color="#0dc5c1"),
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
                                   leafletOutput(outputId="map", height="auto", width="auto")
                                     %>% withSpinner(color="#0dc5c1"),
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
                        dataTableOutput("dataTable") %>% withSpinner(color="#0dc5c1")
                    )
                )
            )
        )))
    )
)
