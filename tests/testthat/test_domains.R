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


##################
context("domainGrid")
##################

test_that("domainGrid can be instanciated", {
  expect_s4_class(domainGridClass(), "domainGrid")
})

randomDomainGrid <- function(nMax=1000) {
  proj <- domainProjectionClass(
    name="lcc",
    lon0=runif(1, -180, 180),
    lat0=runif(1, -90, 90)
  )

  # Generate grid with odd divisions so that center (lon0, lat0) is included
  # Remember:
  #   (i)  The grid point is at the beginning os the mesh
  #   (ii) xmax is located gridResolutionKm after the last grid point
  gridResolutionKm <- 2.5
  nx <- sample(seq.int(1, nMax, 2), 1)
  ny <- sample(seq.int(1, nMax, 2), 1)
  xmax <- (nx + 1) * gridResolutionKm
  ymax <- (ny + 1) * gridResolutionKm

  xaxis <- gridAxisConfigClass(start=-xmax, end=xmax, npts=nx)
  yaxis <- gridAxisConfigClass(start=-ymax, end=ymax, npts=ny)

  return(domainGridClass(xaxis=xaxis, yaxis=yaxis, proj=proj))
}

test_that("lonlat2grid works", {
  domainGrid <- randomDomainGrid()
  proj <- domainGrid$proj
  nx <- domainGrid$nx
  ny <- domainGrid$ny
  xmax <- domainGrid$xmax
  ymax <- domainGrid$ymax

  # Assert that (lon0, lat0) is located at the middle of the grid
  ijGrid <- domainGrid$lonlat2grid(lon=proj$lon0, lat=proj$lat0)
  expect_equal(ijGrid[1,], c((nx+1)/2, (ny+1)/2))

  # Assert that (max_lon, max_lat) correspond to (xmax, ymax)
  lonlatMax <- proj$xy2lonlat(xmax-1E-8, ymax-1E-8)
  ijGrid <- domainGrid$lonlat2grid(lon=lonlatMax$lon, lat=lonlatMax$lat)
  expect_equal(ijGrid[1,], c(nx, ny))
})
