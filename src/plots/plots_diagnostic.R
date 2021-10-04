.diagosticPltColors <- c(
  "obs"="black",
  "fg"="red",
  "an"="green",
  "biascrl"="blue",
  "rawobs"="brown"
)

.diagosticPltLabels <- c(
  "obs"="Observation",
  "fg"="First Guess",
  "an"="Analysis",
  "fg_dep"="First Guess Departure",
  "an_dep"="Analysis Departure",
  "biascrl"="Bias correction",
  "rawobs"="Raw observation"
)

.renamePlotlyTraces <- function(plot, labels) {
  # Renames the traces in plotly plot "plot" according to the
  # namesd list passed as "labels". Leaves trace names intact if
  # they are not listed in "labels".
  iTrace <- 0
  for(pData in plot$x$data) {
    if(pData$name == "") next
    iTrace <- iTrace + 1
    traceLabel <- labels[[pData$name]]
    if(is.null(traceLabel)) next
    plot <- plot %>% style(name=traceLabel, traces=iTrace)
  }
  return(plot)
}

.statPanelStatic <- function(data, column, fill) {
  # Statistics panel (lower panel)
  if(!isTRUE(nrow(data) > 0)) return(ggplot() + theme_void())

  columnName <- substitute(column)
  xLabelPlot <- .diagosticPltLabels[[as.character(columnName)]]
  if(is.null(xLabelPlot)) xLabelPlot <- as.character(columnName)
  rtn <- eval(substitute({
    hist <- ggplot(data) +
      geom_histogram(
        aes(x=column), colour="black", fill=fill,
        binwidth=diff(range(data$fg_dep, data$an_dep))/20.
      ) +
      geom_vline(xintercept = 0.0) +
      xlab(xLabelPlot) + ylab("Count")
    ecdf <- ggplot(data) +
      aes(x=column) +
      stat_ecdf() +
      aes(y=pnorm(column, sd=sd(column))) +
      geom_line(alpha=.4) +
      xlab(xLabelPlot) + ylab("ECDF")
    qq <- ggplot(data) +
      aes(sample=column) +
      stat_qq() +
      xlab(sprintf("%s: Theoretical Quantiles", xLabelPlot)) +
      ylab("Sample Quantiles")
    grid.arrange(hist, ecdf, qq, ncol=3)
  }, list(column=columnName)))
  return(rtn)
}

.statPanelPlotly <- function(data, column, fill) {suppressWarnings({
  # Statistics panel (lower panel) -- Interactive version
  # Suppressing warnings for a reason similar to the one discussed at
  # <https://github.com/ropensci/plotly/issues/1299>
  if(nrow(data)==0) return(plotly_empty(type="scatter"))

  columnName <- as.character(substitute(column))
  xLabelPlot <- .diagosticPltLabels[[columnName]]
  if(is.null(xLabelPlot)) xLabelPlot <- columnName
  sortedColumn <- sort(data[[columnName]])

  # ax: Common layout configs for all axes
  ax <- list(
    titlefont=list(size=14),
    zeroline = FALSE,
    showline = TRUE,
    mirror = "ticks",
    gridcolor = "gray50",
    gridwidth = 0.5,
    zerolinecolor = "red",
    zerolinewidth = 0.5,
    linecolor = "black",
    linewidth = 1
  )

  hist <- plot_ly(x=data[[columnName]], type="histogram",
    showlegend=FALSE,
    hovertemplate = paste0(
      columnName, ': ', '%{x}<br>',
      'Count: ', '%{y:d}',
      '<extra></extra>'
    ),
    marker=list(
      color=fill,
      line=list(width=0.5, color="black")
    ),
    xbins=list(size=diff(range(data$fg_dep, data$an_dep))/20.)
  ) %>%
    layout(
      xaxis=c(ax, title=xLabelPlot),
      yaxis=c(ax, title="Count")
    )

  ecdf_func <- ecdf(sortedColumn)
  # Expand the range in x a bit in the ECDF plot to prevent it
  # from vanishing if there's only one point
  colMax <- sortedColumn[length(sortedColumn)]
  colMin <- sortedColumn[1]
  xStretch <- max(0.05*(colMax-colMin), 0.5)
  xForEcdf <- c(colMin-xStretch, sortedColumn, colMax+xStretch)
  ecdf_plot <- plot_ly() %>%
    add_trace(x=xForEcdf, y=ecdf_func(xForEcdf),
      showlegend=FALSE,
      hovertemplate = paste0(
        columnName, ': ', '%{x}<br>',
        'ECDF: ', '%{y}',
        '<extra></extra>'
      ),
      type="scatter", mode='lines',
      line=list(color="black", shape="hv")
    ) %>%
    add_trace(x=sortedColumn, y=pnorm(sortedColumn, sd=sd(sortedColumn)),
      hovertemplate = paste0(
        columnName, ': ', '%{x}<br>',
        'pnorm: ', '%{y}',
        '<extra></extra>'
      ),
      showlegend=FALSE,
      type="scatter", mode="lines", line=list(color="gray")
    ) %>%
    layout(
      xaxis=c(ax, title=xLabelPlot),
      yaxis=c(ax, title="ECDF")
    )

  qqData <- qqnorm(sortedColumn)
  qq_plot <- plot_ly(x=qqData$x, y=qqData$y,
    showlegend=FALSE,
    hovertemplate = paste0(
      'Theoretical: ', '%{x}<br>',
      'Sample: ', '%{y}',
      '<extra></extra>'
    ),
    type="scatter", mode='markers', marker=list(color="black"),
    ) %>%
    layout(
      xaxis=c(ax, title=sprintf("%s: Theoretical Quantiles", xLabelPlot)),
      yaxis=c(ax, title="Sample Quantiles")
    )

  rtn <- plotly::subplot(hist, ecdf_plot, qq_plot,
    nrows=1,
    margin=c(0.06, 0, 0, 0), # left, right, top and bottom
    titleX=TRUE,
    titleY=TRUE
  )
  return(rtn)
})}

.comparisonPlotStatic <- function(plot) {
  # Comparison plot (upper panel)
  plotData <- plot$data
  if(!isTRUE(nrow(plotData) > 0)) return(ggplot() + theme_void())

  compDf <- data.frame("Date"=plotData[["DTG"]],
                       "obs"=plotData[["obsvalue"]],
                       "fg"=plotData[["obsvalue"]]-plotData[["fg_dep"]])

  hasMinimization <- isTRUE(
    plot$paramsAsInUiInput$odbBase %in% c("ecma_sfc", "ccma")
  )
  if (hasMinimization) {
    compDf["an"] <- plotData[["obsvalue"]]-plotData[["an_dep"]]
  }
  compDf$panel <- "Comparison"

  if ("biascrl" %in% names(plotData)) {
    bias <- plotData[["biascrl"]]
    compDf["rawobs"] <- plotData[["obsvalue"]]+bias
    biasDf <- data.frame("Date"=plotData[["DTG"]],
                         "biascrl"=bias)
    biasDf$panel <- "Bias"
    dfs <- list(compDf, biasDf)
  } else {
      dfs <- list(compDf)
  }
  varname <- unique(plotData$varname)
  data <- do.call(rbind, lapply(dfs, partial(melt, id=c("Date", "panel"))))
  data$panel <- factor(data$panel, levels=c("Comparison", "Bias"))
  plot <- ggplot(data, aes(Date, value, group=variable, colour=variable)) +
    geom_point() +
    facet_grid(panel~., scales="free_y") +
    scale_color_manual(labels=.diagosticPltLabels, values=.diagosticPltColors) +
    labs(y=sprintf("%s [%s]", varname, units(plot$dataWithUnits[["obsvalue"]]))) +
    theme(legend.title=element_blank())
  return(plot)
}

.comparisonPlotPlotly <- function(plot) {
  plotData <- plot$data
  if(nrow(plotData)==0) return(plotly_empty(type="scatter"))

  # Comparison plot (upper panel) -- interactive version
  varname <- unique(plotData$varname)

  # Using empty plots as a trick to get the axes' labels, as well as
  # the legends, to appear where they are supposed to in the final plot.
  # ggplotly does not take care of this and the final plot ends up looking
  # very different from the ggplot one if we don't do this.
  emptyPlot <- plotly_empty(type="scatter", mode='markers') %>%
    layout(
      xaxis=list(title="No Title", color="rgba(0, 0, 0, 0)"),
      yaxis=list(
        yanchor="center", y=0.5,
        title=sprintf("%s [%s]", varname, units(plot$dataWithUnits[["obsvalue"]])),
        titlefont=list(size=14)
      )
    )
  emptyPlot2 <- plotly_empty(type="scatter", mode='markers') %>%
    layout(
      xaxis=list(title="No Title", color="rgba(0, 0, 0, 0)"),
      yaxis=list(title="No Title", color="rgba(0, 0, 0, 0)")
    )

  interactivePanel <- ggplotly(
    .comparisonPlotStatic(plot), tooltip=c("Date","value"),
  ) %>%
    layout(xaxis=list(title="Date")) %>%
    .renamePlotlyTraces(.diagosticPltLabels)

  plotWidth <- 0.9 # Visible plot's relative width. It'll be left-adjusted.
  plot <- plotly::subplot(emptyPlot, interactivePanel, emptyPlot2,
    nrows=1, widths=c(0, plotWidth, 1.0-plotWidth), which_layout=1,
    shareY=FALSE, shareX=FALSE, titleY=TRUE, titleX=TRUE
  ) %>%
    layout(
      legend=list(
        orientation="v",
        xanchor="left", x=plotWidth+0.001,
        yanchor="center", y=0.9
      )
    )
  return(plot)
}

.getStaticStatDiagPlot <- function(plot) {
  panels <- list(
    .comparisonPlotStatic(plot),
    .statPanelStatic(plot$data, fg_dep, .diagosticPltColors[["fg"]])
  )
  hasMinimization <- isTRUE(
    plot$paramsAsInUiInput$odbBase %in% c("ecma_sfc", "ccma")
  )
  if (hasMinimization) {
    lay <- rbind(c(1),
                 c(1),
                 c(2),
                 c(3))
    panels <- c(
      panels,
      list(.statPanelStatic(plot$data, an_dep, .diagosticPltColors[["an"]]))
    )
  } else {
    lay <- rbind(c(1),
                 c(2))
  }
  return(grid.arrange(grobs=panels, layout_matrix=lay))
}

.getInteractiveStatDiagPlot <- function(plot) {
  plotData <- plot$data
  hasMinimization <- isTRUE(
    plot$paramsAsInUiInput$odbBase %in% c("ecma_sfc", "ccma")
  )
  if (hasMinimization) {
    obplot <- plotly::subplot(
      .comparisonPlotPlotly(plot),
      .statPanelPlotly(plotData, fg_dep, .diagosticPltColors[["fg"]]),
      .statPanelPlotly(plotData, an_dep, .diagosticPltColors[["an"]]),
      heights=c(0.40, 0.30, 0.30),
      margin=c(0, 0, 0.1125, 0), # left, right, top and bottom
      titleX=TRUE, titleY=TRUE,
      nrows=3
    )
  } else {
    obplot <- plotly::subplot(
      .comparisonPlotPlotly(plot),
      .statPanelPlotly(plotData, fg_dep, .diagosticPltColors[["fg"]]),
      heights=c(0.45, 0.55),
      margin=c(0, 0, 0.1125, 0), # left, right, top and bottom
      titleX=TRUE, titleY=TRUE,
      nrows=2
    )
  }
  obplot <- obplot %>%
    layout(
      showlegend=TRUE,
      margin = list(t=80, l=5)
    )
  return(obplot)
}

statDiagPlottingFunction <- function(plot) {
  withCallingHandlers({
    if(plot$parentType$interactive) return (.getInteractiveStatDiagPlot(plot))
    return (.getStaticStatDiagPlot(plot))
  },
    warning = function(w) {
      # Suppress annoying "Can only have one: config" warnings
      if (startsWith(conditionMessage(w), "Can only have one: config")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

plotRegistry$registerPlotType(
  name="Station Diagnostics",
  category="Diagnostics",
  dateType="range",
  stationChoiceType="single",
  dataFieldsInRetrievedPlotData=list(
    "DTG", "varname", "obsvalue", "fg_dep", "an_dep", "biascrl", "statid"
  ),
  dataFieldsInSqliteWhereClause=list("statid", "obname", "varname"),
  plottingFunction=statDiagPlottingFunction
)
