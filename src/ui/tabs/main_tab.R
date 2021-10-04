shinyInput_label_embed_caching_icon <- function(tag, ...) {
  # Add an icon & popover to signal that caching is ongoing
  shinyInput_label_embed(
    tag,
    tags$span(
      hidden(shiny::icon("sync", class="fa-spin updating_info_icon")) %>%
        bs_embed_tooltip(title="Updating choices...", trigger="hover"),
      hidden(shiny::icon("spinner", class="fa-spin caching_info_icon")) %>%
        bs_embed_popover(
          title="Caching ongoing...",
          content="Choices may change while this happens",
          trigger="hover"
        )
    ),
    ...
  )
}

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
      use_bs_popover(),
      pickerInput("experiment",
        label=getDefLabel("experiment"),
        choices=c(),
        options=list(`live-search`=TRUE, size=10)
      ),
      pickerInput("odbBase",
        label=getDefLabel("odbBase"),
        choices=c()
      ),
      conditionalPanel(
        condition = "input.odbBase!='ecma_sfc'",
        pickerInput("obtype",
          label=getDefLabel("obtype"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype!='satem' &&
                     input.obtype!='scatt' &&
                     input.obtype!='radar'",
        pickerInput("obname",
          label=getDefLabel("obname"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      conditionalPanel(
        condition = "input.obtype == 'satem'",
        pickerInput("sensor",
          label=getDefLabel("sensor"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon(),
        pickerInput("satellite",
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
        pickerInput("scatt_satellite",
          label=getDefLabel("satellite"),
          choices=c()
        ) %>% shinyInput_label_embed_caching_icon()
      ),
      fluidRow(
        column(6,
          pickerInput("variable",
            label=getDefLabel("variable"),
            choices=c(),
          ) %>% shinyInput_label_embed_caching_icon()
        ),
        column(6,
          textInput(
            "variableUnits",
            label="Units",
            value=character(0),
            placeholder="(optional)"
          )
        )
      ),
      conditionalPanel(
        condition = "input.odbBase != 'ecma_sfc' &&
                     input.obtype!='scatt' &&
                     input.obtype != 'satem' &&
                     input.obtype!='surface'",
        fluidRow(
          column(6,
            pickerInput("levels",
              label=getDefLabel("levels"),
              choices=c(),
              multiple=TRUE,
              options=list(
                `live-search`=TRUE,
                `actions-box`=TRUE,
                `none-selected-text`="Any",
                `select-all-text`="All Listed",
                `deselect-all-text`="Don't filter"
              )
            ) %>% shinyInput_label_embed_caching_icon()
          ),
          column(6,
            textInput(
              "levelsUnits",
              label="Units",
              value=character(0),
              placeholder="(optional)"
            )
          )
        ),
        hidden(materialSwitch(
          inputId='standardLevelsSwitch',
          label="List Standard Levels Only",
          status="warning",
          inline=TRUE,
          right=TRUE
        )),
        hidden(
          shinyInput_label_embed(
            materialSwitch(
              inputId='groupLevelsIntoStandardSwitch',
              label="Standardise Levels in the Final Plot",
              status="warning",
              inline=TRUE,
              right=TRUE
            ),
            tags$span(
              HTML('&emsp;'),
              shiny::icon("info") %>%
                bs_embed_tooltip(
                  title=paste(
                    "Change the values of the plotted levels to the",
                    "nearest corresponding standard/reference levels."
                  ),
                  placement="right"
                )
            )
          )
        )
      ),
      pickerInput("plottype",
        label=getDefLabel("plottype"),
        choices=c(),
        options=list(`dropup-auto`=FALSE, `live-search`=TRUE, size=10)
      ),
      hidden(numericInput("minNobsForGriddedAverages",
        label=HTML("Gridded Average: Min &numero; Obs per Grid Element"),
        value=1,
        min=1,
        step=1
      )),
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
      tags$div(
        class="single_dtg_inputs",
        fluidRow(
          column(8, dateInput("date", "Date")),
          column(4, pickerInput("cycle", label="Cycle", choices=c()))
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
    mainPanel=mainPanel(width=9, tabsetPanel(id="mainAreaTabsetPanel"))
  )
)}
