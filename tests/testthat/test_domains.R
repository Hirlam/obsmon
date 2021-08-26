workingDir <- getwd()
untar("test_data/mock_experiment.tgz", exdir="test_data")
setwd("../..")
sink("/dev/null")
source("src/init.R")
sink()
setwd(workingDir)

##################
context("gridAxisConfig")
##################

test_that("gridAxisConfig can be instanciated", {
  expect_s4_class(gridAxisConfigClass(), "gridAxisConfig")
})

test_that("gridAxisConfig npts and npts_ezone are correctly set", {
  npts <- sample(1:100, 1)
  expect_equal(gridAxisConfigClass(npts=npts)$npts, npts) 
  expect_equal(gridAxisConfigClass(npts_ezone=npts)$npts_ezone, npts)
})

test_that("gridAxisConfig npts and npts_ezone are correctly set", {
  start <- runif(1, 0.0, 100)
  end <- start + 1.0
  gridAxisConfig <- gridAxisConfigClass(start=start, end=end)
  expect_equal(gridAxisConfig$start, start) 
  expect_equal(gridAxisConfig$end, end) 

  start <- runif(1, 0.0, 100)
  end <- start - 1.0
  gridAxisConfig <- gridAxisConfigClass(start=start, end=end)
  expect_equal(gridAxisConfig$start, end) 
  expect_equal(gridAxisConfig$end, start) 
})

test_that("gridAxisConfig npts cannot be smaller than one", {
  expect_error(gridAxisConfigClass(npts=0), "npts must be larger than zero")
  expect_error(gridAxisConfigClass(npts=-1), "npts must be larger than zero")
})

test_that("gridAxisConfig npts_ezone cannot be smaller than zero", {
  expect_equal(gridAxisConfigClass()$npts_ezone, 0)
  expect_equal(gridAxisConfigClass(npts_ezone=0)$npts_ezone, 0)
  expect_error(
    gridAxisConfigClass(npts_ezone=-1),
    "npts_ezone must be larger than or equal to zero"
  )
})


##################
context("grid2D")
##################

test_that("grid2D can be instanciated", {
  expect_s4_class(grid2DClass(), "grid2D")
})


##################
context("domainProjection")
##################

test_that("domainProjection can be instanciated", {
  expect_s4_class(domainProjectionClass(), "domainProjection")
})

test_that("lonlat2xy and xy2lonlat work", {
  domainProjection <- domainProjectionClass(name="lcc", lon0=15.0, lat0=63.0)
  nPoints <- sample(5:50, 1)
  lonlatDataframeStart <- data.frame(
    lon=runif(nPoints, -180, 180),
    lat=runif(nPoints, -90, 90)
  )
  xyDataFrame <- domainProjection$lonlat2xy(
    lon=lonlatDataframeStart$lon,
    lat=lonlatDataframeStart$lat
  )
  lonlatDataframeEnd <- domainProjection$xy2lonlat(
    x=xyDataFrame$x,
    y=xyDataFrame$y
  )
  expect_equal(lonlatDataframeStart, lonlatDataframeEnd)
})
