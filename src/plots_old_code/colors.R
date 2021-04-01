getSuitableColorScale <- function(plotData) {
  # Use a divergent colormat whenever there's change of sign
  # in the data, but use a sequential colormap otherwise.
  # Red/blue colors will represent +/- values.
  cm <- list(name=NULL, palette=NULL, direction=NULL, domain=NULL)
  dataRange <- range(plotData$plotValues)
  if (prod(dataRange) >= 0) {
    spread <- diff(dataRange)
    if (sign(sum(dataRange)) < 0) {
      cm$name <- "Blues"
      cm$direction <- -1
      mincol <- dataRange[1]
      snapToZero <- dataRange[2]^2 < spread
      maxcol <- ifelse(snapToZero, 0., dataRange[2])
    } else {
      cm$name <- "Reds"
      cm$direction <- 1
      maxcol <- dataRange[2]
      snapToZero <- dataRange[1]^2 < spread
      mincol <- ifelse(snapToZero, 0., dataRange[1])
    }
  } else {
    cm$name <- "RdBu"
    cm$direction <- -1
    maxcol <- max(abs(dataRange))
    mincol <- -maxcol
  }
  cm$palette <- brewer.pal(brewer.pal.info[cm$name,]$maxcolors, cm$name)
  if(cm$direction < 0) cm$palette <- rev(cm$palette)
  cm$domain <- c(mincol, maxcol)
  return(cm)
}
