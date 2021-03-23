
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

mainTab <- function() {fluidPage(
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectizeInput("experiment",
        multiple=FALSE,
        label=getDefLabel("experiment"),
        options=list(placeholder='Loading experiments...'),
        choices=c()
      ),
      selectInput("odbBase",
        label=getDefLabel("odbBase"),
        multiple=FALSE,
        choices=c()
      ),
      conditionalPanel(
        condition = "input.odbBase!='ecma_sfc'",
        selectInput("obtype",
          label=getDefLabel("obtype"),
          choices=c()
        )
      ),
      conditionalPanel(
        condition = "input.obtype!='satem' &&
                     input.obtype!='scatt' &&
                     input.obtype!='radar'",
        selectInput("obname",
          label=getDefLabel("obname"),
          choices=c()
        )
      ),
      conditionalPanel(
        condition = "input.obtype == 'satem'",
        selectInput("sensor",
          label=getDefLabel("sensor"),
          choices=c()
        ),
        selectInput("satellite",
          label=getDefLabel("satellite"),
          choices=c()
        ),
        selectInput("channels",
          tags$div(getDefLabel("channels"),
            "(Select",
            actionLink("channelsSelectAny", "any"),
            ")"
          ),
          choices=c(),
          multiple=TRUE,
          selectize=FALSE
        )
      ),
      conditionalPanel(
        condition = "input.obtype == 'scatt'",
        selectInput("scatt_satellite",
          label=getDefLabel("satellite"),
          choices=c()
        )
      ),
      conditionalPanel(
        condition = "input.obtype != 'satem'",
        selectInput("variable",
          label=getDefLabel("variable"),
          choices=c()
        ),
        conditionalPanel(
          condition = "input.odbBase != 'ecma_sfc' &&
                       input.obtype!='scatt' &&
                       input.obtype!='surface'",
          pickerInput("levels",
            label=getDefLabel("levels"),
            choices=c(),
            multiple=TRUE,
            options=list(
              `live-search`=TRUE,
              `actions-box`=TRUE,
              `none-selected-text`="Any",
              `select-all-text`="Select All Listed",
              `deselect-all-text`='Select "Any" (no level filters)'
            )
          ),
          hidden(materialSwitch(
            inputId='standardLevelsSwitch',
            label="List Standard Levels Only",
            status="warning",
            inline=TRUE,
            right=TRUE
          ))
        )
      ),
      selectInput("plottype",
        label=getDefLabel("plottype"),
        choices=c()
      ),
      pickerInput("station",
        label=getDefLabel("station"),
        choices=c(),
        multiple=TRUE,
        options=list(
          `live-search`=TRUE,
          `actions-box`=TRUE,
          `none-selected-text`="Any",
          `select-all-text`="Select All Listed",
          `deselect-all-text`='Select "Any" (no station filters)'
        )
      ),
      pickerInput("stationSingle",
        label=getDefLabel("station"),
        choices=c(),
        multiple=FALSE,
        options=list(
          `live-search`=TRUE,
          `none-selected-text`="No station selected"
        )
      ),
      fluidRow(
        column(8, dateInput("date", "Date")),
        column(4, selectInput("cycle", label="Cycle", choices=c()))
      ),
      dateRangeInput("dateRange", "Date Range"),
      checkboxGroupInput("cycles",
        label=tags$div("Cycles",
          "(Select",
          actionLink("cyclesSelectAll", "all"),
          actionLink("cyclesSelectAny", "any"),
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
    mainPanel=createMainPanel()
  )
)}
