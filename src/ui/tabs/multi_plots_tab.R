multiPlotsTab <- function() {
  sidebarLayout(
    sidebarPanel(
      width=3,
      selectizeInput(
        "multiPlotTitle",
        multiple=FALSE,
        label="MultiPlot Name",
        options=list(placeholder='Please select multiPlot'),
        choices=c()
      ),
      actionButton("multiPlotsDoPlot", "Make multiPlot",
        width="100%",
        icon("ok", lib="glyphicon")
      ),
      shinyjs::hidden(actionButton("multiPlotsCancelPlot", "Cancel multiPlot",
        width="100%",
        icon("remove", lib="glyphicon"),
        style="color: #fff; background-color: #FFA500; border-color: #2e6da4"
      ))
    ),
    mainPanel=mainPanel(width=9, uiOutput(outputId="multiPlotTabsetPanelsContainer"))
  )
}
