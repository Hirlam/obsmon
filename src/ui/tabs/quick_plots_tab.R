quickPlotsTab <- function() {
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectizeInput(
        "quickPlotTitle",
        multiple=FALSE,
        label="Plot title",
        options=list(placeholder='Please select plot'),
        choices=c()
      ),
      actionButton("quickPlotsDoPlot", "Plot",
        width="100%",
        icon("ok", lib="glyphicon")
      ),
      shinyjs::hidden(actionButton("quickPlotsCancelPlot", "Cancel plot",
        width="100%",
        icon("remove", lib="glyphicon"),
        style="color: #fff; background-color: #FFA500; border-color: #2e6da4"
      ))
    ),
    mainPanel=createMainPanel(prependToIds="quickPlots")
  )
}
