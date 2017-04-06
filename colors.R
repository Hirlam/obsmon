colormapContinuous <- function(minval, maxval) {
  cm <- list()
  if (minval*maxval >= 0) {
    cm$type <- "seq"
    spread <- maxval - minval
    dataSign <- sign(maxval + minval)
    if (dataSign < 0) {
      cm$palette <- "Blues"
      direction <- -1
      mincol <- minval
      snapToZero <- maxval^2 < spread
      maxcol <- ifelse(snapToZero, 0., maxval)
    } else {
      cm$palette <- "Reds"
      direction <- 1
      maxcol <- maxval
      snapToZero <- minval^2 < spread
      mincol <- ifelse(snapToZero, 0., minval)
    }
  } else {
    cm$type <- "div"
    cm$palette <- "RdBu"
    direction <- -1
    col <- max(abs(minval), abs(maxval))
    mincol <- -col
    maxcol <- col
  }
  cm$direction <- direction
  cm$reverse <- direction < 0
  cm$domain <- c(mincol, maxcol)
  cm
}
