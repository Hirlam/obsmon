registerPlotCategory("Diagnostic")

statPanel <- function(data, column, bw, fill) {
  # Statistics panel
  columnName <- substitute(column)
  rtn <- eval(substitute({
    hist <- ggplot(data) +
      geom_histogram(aes(x=column), colour="black", fill=fill, binwidth=bw) +
      geom_vline(xintercept = 0.0)
    ecdf <- ggplot(data) +
      aes(x=column) +
      stat_ecdf() +
      aes(y=pnorm(column, sd=sd(column))) +
      geom_line(alpha=.4) +
      ylab("ECDF")
    qq <- ggplot(data) +
      aes(sample=column) +
      stat_qq()
    grid.arrange(hist, ecdf, qq, ncol=3)
  }, list(column=columnName)))
  return(rtn)
}

statPanelPlotly <- function(data, column, bw, fill) {
  # Statistics panel (interactive version)
  columnName <- as.character(substitute(column))
  sortedColumn <- sort(data[[columnName]])

  ax <- list(
    titlefont=list(size=14),
    zeroline = FALSE,
    showline = TRUE,
    mirror = "ticks",
    gridcolor = "gray90",
    gridwidth = 0.5,
    zerolinecolor = "red",
    zerolinewidth = 0.5,
    linecolor = "black",
    linewidth = 1
  )

  hist <- plot_ly(x=data[[columnName]], type="histogram",
    showlegend=FALSE,
    hovertemplate = paste0(columnName, ': ', '%{x}<br>', 'Count: ', '%{y:d}'),
    marker=list(
      color=fill,
      line=list(width=0.5, color="black")
    ),
    xbins=list(size=bw)
  ) %>%
    layout(xaxis=ax, yaxis=ax) %>%
    layout(
      xaxis=list(title=columnName),
      yaxis=list(title="Count")
    )

  ecdf_func <- ecdf(sortedColumn)
  ecdf <- plot_ly() %>%
    add_trace(x=sortedColumn, y=ecdf_func(sortedColumn),
      showlegend=FALSE,
      hovertemplate = paste0(columnName, ': ', '%{x}<br>', 'ECDF: ', '%{y}'),
      type="scatter", mode='lines',
      line=list(color="black", shape="hv")
    ) %>%
    add_trace(x=sortedColumn, y=pnorm(sortedColumn, sd=sd(sortedColumn)),
      hovertemplate = paste0(columnName, ': ', '%{x}<br>', 'pnorm: ', '%{y}'),
      showlegend=FALSE,
      type="scatter", mode="lines", line=list(color="gray")
    ) %>%
    layout(xaxis=ax, yaxis=ax) %>%
    layout(
      xaxis=list(title=columnName),
      yaxis=list(title="ECDF")
    )

  qqData <- qqnorm(sortedColumn)
  qq <- plot_ly(x=qqData$x, y=qqData$y,
    showlegend=FALSE,
    hovertemplate = paste0('Theoretical: ', '%{x}<br>', 'Sample: ', '%{y}'),
    type="scatter", mode='markers', marker=list(color="black"),
    ) %>%
    layout(xaxis=ax, yaxis=ax) %>%
    layout(
      xaxis=list(title="Theoretical"),
      yaxis=list(title="Sample")
    )

  rtn <- plotly::subplot(hist, ecdf, qq,
    nrows=1,
    margin=c(0.06, 0, 0, 0), # left, right, top and bottom
    titleX=TRUE,
    titleY=TRUE
  )
  return(rtn)
}


plotTitle.plotDiagnostic <- function(p, plotRequest, plotData) {
  crit <- plotRequest$criteria
  stationLabel <- getStationsForPlotTitle(plotRequest, plotData)
  title <- sprintf(
    "%s: %s\nstation=%s\ndb=%s, DTG=%s, obname=%s, varname=%s",
    plotRequest$expName, p$name,
    stationLabel,
    plotRequest$dbType, formatDtg(crit$dtg), crit$obname, crit$varname
  )
  return(title)
}

doPlot.plotDiagnostic <- function(
  p, plotRequest, plotData, interactive=FALSE
) {
  info <- list()
  info$labels <- c("obs"="Observation",
                   "fg"="First Guess",
                   "an"="Analysis",
                   "biascrl"="Bias correction",
                   "rawobs"="Raw observation")
  info$colors <- c("obs"="black",
                   "fg"="red",
                   "an"="green",
                   "biascrl"="blue",
                   "rawobs"="brown")
  dtg <- plotData[["DTG"]]
  obs <- plotData[["obsvalue"]]
  fgDep <- plotData[["fg_dep"]]
  anDep <- plotData[["an_dep"]]
  compDf <- data.frame("Date"=dtg,
                       "obs"=obs,
                       "fg"=obs-fgDep)
  hasMinimization <- plotRequest$dbType %in% c("ecma_sfc", "ccma")
  if (hasMinimization) {
    compDf["an"] <- obs-anDep
  }
  compDf$panel <- "comparison"
  if (plotRequest$criteria$varname=="apd") {
    bias <- plotData[["biascrl"]]
    compDf["rawobs"] <- obs+bias
    biasDf <- data.frame("Date"=dtg,
                         "biascrl"=bias)
    biasDf$panel <- "bias"
    dfs <- list(compDf, biasDf)
  } else {
      dfs <- list(compDf)
  }
  varname <- unique(plotData$varname)
  varnameAndUnits <- sprintf("%s [%s]", varname, units[[varname]])
  data <- do.call(rbind, lapply(dfs, partial(melt, id=c("Date", "panel"))))
  data$panel <- factor(data$panel, levels=c("comparison", "bias"))
  comparison <- ggplot(data, aes(Date, value, group=variable, colour=variable)) +
    geom_point() +
    facet_grid(panel~., scales="free_y") +
    scale_color_manual(labels=info$labels, values=info$colors) +
    labs(y=varnameAndUnits) +
    theme(legend.title=element_blank())
  maxval <- max(plotData$fg_dep, plotData$an_dep)
  minval <- min(plotData$fg_dep, plotData$an_dep)
  bw <- (maxval-minval)/20.

  if(interactive) {
    interactiveComparisonPlot <- ggplotly(comparison, tooltip=c("x", "y")) %>%
      layout(
        legend=list(
          orientation="v",
          xanchor="left", x=1.025,
          yanchor="center", y=0.75
        ),
        # Axis labels vanish with subplot. Adding them back.
        xaxis=list(title="Date", titlefont=list(size=14)),
        yaxis=list(title=varnameAndUnits, titlefont=list(size=14))
      )
    # Constructing the final plot
    fgDepStatPanel <- statPanelPlotly(plotData,fg_dep,bw,info$colors[["fg"]])
    if (hasMinimization) {
      andDepStatPanel<-statPanelPlotly(plotData,an_dep,bw,info$colors[["an"]])
      obplot <- plotly::subplot(
        interactiveComparisonPlot,
        fgDepStatPanel,
        andDepStatPanel,
        heights=c(0.30, 0.35, 0.35),
        titleX=TRUE,
        titleY=TRUE,
        margin=c(0, 0, 0.1125, 0), # left, right, top and bottom
        nrows=3
      )
    } else {
      obplot <- plotly::subplot(
        interactiveComparisonPlot,
        fgDepStatPanel,
        heights=c(0.45, 0.55),
        titleX=TRUE,
        titleY=TRUE,
        margin=c(0, 0, 0.1125, 0), # left, right, top and bottom
        nrows=2
      )
    }
    obplot <- obplot %>%
      layout(
        showlegend=TRUE,
        margin = list(t=80, l=80, b=50)
      )
  } else {
    panels <- list(
      comparison,
      statPanel(plotData, fg_dep, bw, info$colors[["fg"]])
    )
    if (hasMinimization) {
      lay <- rbind(c(1),
                   c(1),
                   c(2),
                   c(3))
      panels <- c(
        panels,
        list(statPanel(plotData, an_dep, bw, info$colors[["an"]]))
      )
    } else {
      lay <- rbind(c(1),
                   c(2))
    }
    obplot <- grid.arrange(grobs=panels, layout_matrix=lay)
  }
  obplot
}

doPlotly.plotDiagnostic <- function(...) {
  doPlot.plotDiagnostic(..., interactive=TRUE)
}

registerPlotType(
    "Diagnostic",
    plotCreate("plotDiagnostic", "Station Diagnostics", "range",
               paste("SELECT",
                     "DTG, varname, obsvalue, fg_dep, an_dep, biascrl, statid",
                     "FROM usage WHERE %s"),
               list("station"))
)
