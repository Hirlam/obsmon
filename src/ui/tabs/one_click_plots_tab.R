oneClickPlotsTab <- function() {
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectizeInput(
        "oneClickPlotTitle",
        multiple=FALSE,
        label="Plot title",
        options=list(placeholder='Please select plot'),
        choices=c()
      ),
      actionButton("oneClickPlotsDoPlot", "Plot",
        width="100%",
        icon("ok", lib="glyphicon")
      ),
      shinyjs::hidden(actionButton("oneClickPlotsCancelPlot", "Cancel plot",
        width="100%",
        icon("remove", lib="glyphicon"),
        style="color: #fff; background-color: #FFA500; border-color: #2e6da4"
      ))
    ),
    mainPanel=createMainPanel(prependToIds="oneClickPlots")
  )
}
