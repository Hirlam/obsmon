shinyInput_label_embed_caching_icon <- function(tag, ...) {
  # Add an icon & popover to signal that caching is ongoing
  shinyInput_label_embed(
    tag,
    tags$span(
      shiny::icon("sync", class="fa-spin"), class="caching_info_icon"
    ) %>%
    bs_embed_popover(
      title="Updating...",
      content = "Caching ongoing. Values may change while this happens."
    ),
    ...
  )
}
use_bs_popover()

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
        options=list(placeholder='Select experiment'),
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
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype!='satem' &&
                     input.obtype!='scatt' &&
                     input.obtype!='radar'",
        selectInput("obname",
          label=getDefLabel("obname"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype == 'satem'",
        selectInput("sensor",
          label=getDefLabel("sensor"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon(),
        selectInput("satellite",
          label=getDefLabel("satellite"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon(),
        pickerInput("channels",
          label=getDefLabel("channels"),
          choices=c(),
          multiple=TRUE,
          options=list(
            `live-search`=TRUE,
            `actions-box`=TRUE,
            `none-selected-text`="Any",
            `select-all-text`="Select All Listed",
            `deselect-all-text`='Select "Any" (no channel filters)'
          )
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype == 'scatt'",
        selectInput("scatt_satellite",
          label=getDefLabel("satellite"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype != 'satem'",
        selectInput("variable",
          label=getDefLabel("variable"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon(),
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
          ) %>% shinyInput_label_embed_caching_icon(),
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
      hidden(pickerInput("station",
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
      )) %>% shinyInput_label_embed_caching_icon(),
      hidden(pickerInput("stationSingle",
        label=getDefLabel("station"),
        choices=c(),
        multiple=FALSE,
        options=list(
          `live-search`=TRUE,
          `none-selected-text`="No station selected"
        )
      )) %>% shinyInput_label_embed_caching_icon(),
      tags$div(
        class="single_dtg_inputs",
        fluidRow(
          column(8, dateInput("date", "Date")),
          column(4, selectInput("cycle", label="Cycle", choices=c()))
        )
      ),
      hidden(tags$div(
        class="multiple_dtg_inputs",
        fluidRow(
          column(8, dateRangeInput("dateRange", "Date Range")),
          column(4,
            pickerInput("cycles",
              label=getDefLabel("cycles"),
              choices=c(),
              multiple=TRUE,
              options=list(
                `live-search`=TRUE,
                `actions-box`=TRUE,
                `none-selected-text`="Any",
                `select-all-text`="All",
                `deselect-all-text`="Any"
              )
            )
          )
        )
      )),
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
