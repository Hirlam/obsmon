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
  expect_equal(c(ijGrid$i, ijGrid$j), c((nx+1)/2, (ny+1)/2))

  # Assert that (max_lon, max_lat) correspond to (xmax-dx, ymax-dy)
  lonlatMax <- proj$xy2lonlat(
    xmax - domainGrid$x_spacing*1E-3,
    ymax - domainGrid$y_spacing*1E-3
  )
  ijGrid <- domainGrid$lonlat2grid(lon=lonlatMax$lon, lat=lonlatMax$lat)
  expect_equal(c(ijGrid$i, ijGrid$j), c(nx, ny))
})

test_that("grid2lonlat works", {
  domainGrid <- randomDomainGrid()
  ijData <- data.frame(
    i=sample.int(domainGrid$nx, 200, replace=TRUE),
    j=sample.int(domainGrid$ny, 200, replace=TRUE)
  )
  lonlatData <- domainGrid$grid2lonlat(i=ijData$i, j=ijData$j)
  ijNewData <- domainGrid$lonlat2grid(lon=lonlatData$lon, lat=lonlatData$lat)
  expect_equal(ijData, ijNewData)
})

test_that("corners_lonlat works", {
  domainGrid <- randomDomainGrid()
  cornersXY <- domainGrid$corners
  cornersLonLat <- domainGrid$corners_lonlat

  diff <- 0.0
  for(i in seq_along(cornersXY)) {
    cornerXY <- cornersXY[[i]]
    cornerLonLat <- cornersLonLat[[i]]
    lonlat <- domainGrid$proj$xy2lonlat(x=cornerXY[1], y=cornerXY[2])
    diff <- diff + (lonlat$lon - cornerLonLat[1])**2
    diff <- diff + (lonlat$lat - cornerLonLat[2])**2
  }

  expect_equal(diff, 0.0)
})

test_that("ezone_corners_lonlat works", {
  domainGrid <- randomDomainGrid()
  ezoneCornersXY <- domainGrid$ezone_corners
  ezoneCornersLonLat <- domainGrid$ezone_corners_lonlat

  diff <- 0.0
  for(i in seq_along(ezoneCornersXY)) {
    cornerXY <- ezoneCornersXY[[i]]
    cornerLonLat <- ezoneCornersLonLat[[i]]
    lonlat <- domainGrid$proj$xy2lonlat(x=cornerXY[1], y=cornerXY[2])
    diff <- diff + (lonlat$lon - cornerLonLat[1])**2
    diff <- diff + (lonlat$lat - cornerLonLat[2])**2
  }

  expect_equal(diff, 0.0)
})

##################
context("domain")
##################

test_that("domain can be instanciated", {
  expect_s4_class(domainClass(), "domain")
})

getMetcoopDomain <- function() {
  config <- list(
    name = "MetCoOp",
    nlon = 900,
    nlat = 960,
    lonc = 16.763011639,
    latc = 63.489212956,
    lon0 = 15.0,
    lat0 = 63.0,
    gsize = 2500.0,
    ezone = 11,
    lmrt = FALSE
  )

  return(domainClass(
    name="MetCoOp Domain",
    center_lonlat=c(config$lonc, config$latc),
    proj_lon0_lat0=c(config$lon0, config$lat0),
    lmrt=config$lmrt,
    ngrid_lonlat=c(config$nlon, config$nlat),
    grid_spacing=config$gsize,
    ezone_ngrid=config$ezone
  ))
}

test_that("Real-case domain can be instanciated", {
  expect_s4_class(getMetcoopDomain(), "domain")
})

test_that("Domain and grid projections are identical", {
  domain <- getMetcoopDomain()
  expect_identical(domain$proj, domain$grid$proj)
})

test_that("drawBoundaries runs", {
  domain <- getMetcoopDomain()
  fig <- drawBoundaries(fig=plotly_empty(), domain=domain)
  expect_s3_class(fig, "plotly")
})
