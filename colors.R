colormapContinuous <- function(minval, maxval) {
  cm <- list()
  if (minval*maxval >= 0) {
    cm$type <- "seq"
    spread <- maxval - minval
    dataSign <- sign(maxval + minval)
    if (dataSign < 0) {
      cm$palette <- "Blues"
      cm$direction <- -1
      cm$mincol <- minval
      snapToZero <- maxval^2 < spread
      cm$maxcol <- ifelse(snapToZero, 0., maxval)
    } else {
      cm$palette <- "Reds"
      cm$direction <- 1
      cm$maxcol <- maxval
      snapToZero <- minval^2 < spread
      cm$mincol <- ifelse(snapToZero, 0., minval)
    }
  } else {
    cm$type <- "div"
    cm$palette <- "RdBu"
    cm$direction <- -1
    col <- max(abs(minval), abs(maxval))
    cm$mincol <- -col
    cm$maxcol <- col
  }
  cm
}
