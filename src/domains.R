# We'll approximate the Earth as a sphere
.EARTH_RADIUS <- 6.3710088E6  # Mean earth radius in meters
.EQUATOR_PERIM = 2.0 * pi * .EARTH_RADIUS

gridAxisConfigClass <- setRefClass(Class="gridAxisConfig",
  # Configs for a grid axis.
  fields=list(
    start="numeric",
    end="numeric",
    npts="numeric",
    npts_ezone="numeric"
  ),
  ############################
  methods=list(
    initialize = function(...) {
      callSuper(...)
      if(length(.self$npts_ezone)==0) .self$npts_ezone <- 0
      if(.self$npts_ezone < 0) stop("npts_ezone must be larger than or equal to zero")
      if(length(.self$npts)>0 && .self$npts<1) stop("npts must be larger than zero")
      if(length(.self$start)>0 && length(.self$end)>0 && .self$end<.self$start) {
        tmp <- .self$end
        .self$end <- .self$start
        .self$start <- tmp
      }
    }
  )
)

grid2DClass <- setRefClass(Class="grid2D",
  # A general grid in two dimensions, named x and y by convention.
  fields=list(
    xaxis="gridAxisConfig",
    yaxis="gridAxisConfig",
    nx=function(...) {return(.self$xaxis$npts)},
    ny=function(...) {return(.self$yaxis$npts)},
    nx_ezone=function(...) {return(.self$xaxis$npts_ezone)},
    ny_ezone=function(...) {return(.self$yaxis$npts_ezone)},
    xmin=function(...) {return(.self$xaxis$start)},
    xmax=function(...) {return(.self$xaxis$end)},
    xlims=function(...) {return(c(.self$xmin, .self$xmax))},
    ymin=function(...) {return(.self$yaxis$start)},
    ymax=function(...) {return(.self$yaxis$end)},
    ylims=function(...) {return(c(.self$ymin, .self$ymax))},
    ezone_xmax=function(...) {
      # Return the max value of x within the projected extension zone."""
      return (.self$xmax + .self$nx_ezone * .self$x_spacing)
    },
    ezone_ymax=function(...) {
      # Return the max value of x within the projected extension zone."""
      return (.self$ymax + .self$ny_ezone * .self$y_spacing)
    },
    x_spacing=function(...) {
      # Return the spacing, in meters, between nearest grid x-coords."""
      return ((.self$xmax - .self$xmin) / .self$nx)
    },
    y_spacing=function(...) {
      # Return the spacing, in meters, between nearest grid y-coords."""
      return ((.self$ymax - .self$ymin) / .self$ny)
    },
    ncells=function(...) {
      # Return the total number of grid cells."""
      return (.self$nx * .self$ny)
    },
    corners=function(...) {
      # Return list of projected (x, y) coords of the grid corners.
      return (list(
        c(.self$xmin, .self$ymin),
        c(.self$xmax, .self$ymin),
        c(.self$xmax, .self$ymax),
        c(.self$xmin, .self$ymax)
      ))
    },
    ezone_corners=function(...) {
      # Return tuple of projected (x, y) coords of the ezone corners.
      return (list(
          c(.self$xmax, .self$ymin),
          c(.self$ezone_xmax, .self$ymin),
          c(.self$ezone_xmax, .self$ezone_ymax),
          c(.self$xmin, .self$ezone_ymax),
          c(.self$xmin, .self$ymax),
          c(.self$xmax, .self$ymax)
      ))
    }
  ),
  #####################
  methods=list(
    cart2grid=function(x, y) {
      # Return grid (i, j) cell containing projected (x, y) coords.
      if(length(x) != length(y)) stop("x and y must have same length")
      cart2grid_non_vec <- function(x, y) {
	i <- 1 + as.integer((x - .self$xmin) / .self$x_spacing)
	j <- 1 + as.integer((y - .self$ymin) / .self$y_spacing)
	if (i < 0 || i > .self$ncells - 1) i <- NA
	if (j < 0 || j > .self$ncells - 1) j <- NA
	return (c(i, j))
      }
      cart2grid_vectorised <- Vectorize(cart2grid_non_vec)
      return(t(cart2grid_vectorised(x, y)))
    },
    ij2xy_map=function(...) {
      # Return grid2x, grid2y such that x=grid2x[i, j], y=grid2y[i, j].
      # We can then work with x=grid2x[i, j], y=grid2y[i, j]
      xvals <- seq(.self$xmin, .self$xmax, length.out=.self$nx+1)
      yvals <- seq(.self$ymin, .self$ymax, length.out=.self$ny+1)
      xvals <- xvals[1:length(xvals)-1]
      yvals <- yvals[1:length(yvals)-1]
      grid2x <- t(matrix(rep(xvals, length(yvals)), ncol=length(xvals), byrow=TRUE))
      grid2y <- matrix(rep(yvals, length(xvals)), ncol=length(yvals), byrow=TRUE)
      return(list(grid2x=grid2x, grid2y=grid2y))
    }
  )
)

########################################
domainProjectionClass <- setRefClass(Class="domainProjection",
  # Cartographic projection to be used in a domain.
  fields=list(
    name="character",
    lon0="numeric",
    lat0="numeric",
    projparams=function(...) {
      # Return the domain's projparams. See <https://proj.org>.
      params <- list(
        proj=.self$name,
        R=.EARTH_RADIUS,
        lat_0=.self$lat0,
        lon_0=.self$lon0,
        lat_1=.self$lat0,
        lon_1=.self$lon0,
        a=.EARTH_RADIUS,
        b=.EARTH_RADIUS
      )
      proj4string <- paste(paste0("+", names(params)), params, sep="=", collapse=" ")
      return(proj4string)
    }
  ),
  methods=list(
    lonlat2xy=function(lon, lat) {
      # Convert (lon, lat), in degrees, into projected (x, y) in meters.
      xyData <- st_as_sf(
        data.frame(lon=lon, lat=lat),
        coords=c("lon", "lat"),
        crs="WGS84"
      ) %>%
        st_transform(.self$projparams) %>%
        dplyr::mutate(
          x=sf::st_coordinates(.)[,1],
          y=sf::st_coordinates(.)[,2]
        ) %>%
        sf::st_set_geometry(NULL)

      rownames(xyData) <- NULL
      rownames(xyData$x) <- NULL
      rownames(xyData$y) <- NULL

      return(xyData)
    },
    xy2lonlat=function(x, y) {
      # Convert projected (x, y), in meters, into (lon, lat) in degrees.
      lonlatData <- st_as_sf(
        data.frame(x=x, y=y),
        coords=c("x", "y"),
        crs=.self$projparams
      ) %>%
        st_transform("WGS84") %>%
        dplyr::mutate(
          lon=sf::st_coordinates(.)[,1],
          lat=sf::st_coordinates(.)[,2]
        ) %>%
        sf::st_set_geometry(NULL)

      rownames(lonlatData) <- NULL
      rownames(lonlatData$lon) <- NULL
      rownames(lonlatData$lat) <- NULL

      return(lonlatData)
    }
  )
)

########################################
domainGridClass <- setRefClass(Class="domainGrid",
  # A 2D grid with (lon, lat) <--> (x, y) projection awareness.
  contains=c("grid2D"),
  fields=list(
    proj="domainProjection",
    # Number of grid points along the longitude axis.
    nlon=function(...) .self$nx,
    # Number of grid points along the latitude axis.
    nlat=function(...) .self$ny,
    corners_lonlat=function(...) {
      # Return tuple of (lon, lat) coords of the grid corners.
      .xy2lonlat <- function(vec) {
        lonlat <- .self$proj$xy2lonlat(vec[1], vec[2])
        return(c(lonlat$lon, lonlat$lat))
      }
      return(lapply(.self$corners, .xy2lonlat))
    },
    ezone_corners_lonlat=function(...) {
      # Return tuple of (lon, lat) coords of the grid corners.
      .xy2lonlat <- function(vec) {
        lonlat <- .self$proj$xy2lonlat(vec[1], vec[2])
        return(c(lonlat$lon, lonlat$lat))
      }
      return(lapply(.self$ezone_corners, .xy2lonlat))
    }
  ),
  methods=list(
    lonlat2grid=function(lon, lat) {
      # Convert (lon, lat) into grid cell coord (i, j).
      xyDataframe <- .self$proj$lonlat2xy(lon, lat)
      return(.self$cart2grid(xyDataframe$x, xyDataframe$y))
    },
    ij2lonlat_map=function() {
      # Return g2lon, g2lat such that lon=g2lon[i, j], lat=g2lat[i, j].
      # TODO: Optimise this
      ij2xy_map <- .self$ij2xy_map()
      grid2x <- ij2xy_map$grid2x
      grid2y <- ij2xy_map$grid2y
      grid2lon <- matrix(ncol=ncol(grid2x), nrow=nrow(grid2x))
      grid2lat <- matrix(ncol=ncol(grid2y), nrow=nrow(grid2y))
      for(i in seq(1, nrow(grid2x))) {
        for(j in seq(1, ncol(grid2x))) {
          x <- ij2xy_map$grid2x[i, j]
          y <- ij2xy_map$grid2y[i, j]
          lonlat <- .self$proj$xy2lonlat(x=x, y=y)
          grid2lon[i, j] <- lonlat$lon
          grid2lat[i, j] <- lonlat$lat
        }
      }
      return(list(grid2lon=grid2lon, grid2lat=grid2lat))
    }
  )
)

########################################
domainClass <- setRefClass(Class="domain",
  # Model domain geometry and grid.
  #
  # See <https://hirlam.org/trac/wiki/HarmonieSystemDocumentation/ModelDomain>.
  fields=list(
    name="character",
    center_lonlat="numeric", # Def: (0.0, 0.0)
    proj_lon0_lat0="numeric", # Def: (0.0, 0.0)
    lmrt="logical", # Def: FALSE
    ngrid_lonlat="numeric",
    grid_spacing="numeric", # Def: _EQUATOR_PERIM
    ezone_ngrid="numeric", # Def: 0
    tstep="numeric", # Def: 0.0
    # Fields that will be changed at init
    grid="domainGrid",
    # Properties
    proj=function(...) {
      # Return the domain's associated DomainProjection object.
      # Make sure the domain and the domain's grid use the same projection
      return(.self$grid$proj)
    },
    minlon=function(...) {
      # Return the min longitude of the domain's corners.
      return(min(sapply(.self$grid$corners_lonlat, function(item) item[1])))
    },
    minlat=function(...) {
      # Return the min latitude of the domain's corners.
      return(min(sapply(.self$grid$corners_lonlat, function(item) item[2])))
    },
    maxlon=function(...) {
      # Return the max longitude of the domain's corners.
      return(max(sapply(.self$grid$corners_lonlat, function(item) item[1])))
    },
    maxlat=function(...) {
      # Return the min latitude of the domain's corners.
      return(max(sapply(.self$grid$corners_lonlat, function(item) item[2])))
    },
    ezone_minlon=function(...) {
      # Return the min longitude of the domain's ezone corners.
      return(min(sapply(.self$grid$ezone_corners_lonlat, function(item) item[1])))
    },
    ezone_minlat=function(...) {
      # Return the min latitude of the domain's ezone corners.
      return(min(sapply(.self$grid$ezone_corners_lonlat, function(item) item[2])))
    },
    ezone_maxlon=function(...) {
      # Return the max longitude of the domain's ezone corners.
      return(max(sapply(.self$grid$ezone_corners_lonlat, function(item) item[1])))
    },
    ezone_maxlat=function(...) {
      # Return the min latitude of the domain's ezone corners.
      return(max(sapply(.self$grid$ezone_corners_lonlat, function(item) item[2])))
    }
  ),
  methods=list(
    initialize=function(...) {
      # Initialise name, geometry, projection & grid/thinning grid attrs.
      callSuper(...)

      ####################
      # Setting defaults #
      ####################
      defaults <- list(
        name="Unamed Domain",
        center_lonlat=c(0.0, 0.0),
        proj_lon0_lat0=c(0.0, 0.0),
        lmrt=FALSE,
        ngrid_lonlat=c(1, 1),
        grid_spacing=.EQUATOR_PERIM,
        ezone_ngrid=0,
        tstep=0.0
      )
      for(attr in names(defaults)) {
        if(length(.self[[attr]]) == 0) .self[[attr]] <- defaults[[attr]]
      }


      #########################
      # Initialise projection #
      #########################
      if(isTRUE(.self$lmrt) && abs(.self$proj_lon0_lat0[2]) > 0) {
        flog.warn("lat0 should be 0 if lmrt=True. Resetting lat0 to 0.")
        .self$proj_lon0_lat0[2] <- 0.0
      }

      auto_choose_projname <- function() {
        # Define domain projection.
        # Do this in a way close to what is explained at
        # <https://hirlam.org/trac/wiki/HarmonieSystemDocumentation/ModelDomain>

        y_range <- .self$ngrid_lonlat[2] * .self$grid_spacing
        latrange <- 180.0 * y_range / .EQUATOR_PERIM
        if(isTRUE(.self$lmrt) || latrange > 35 || isTRUE(all.equal(latrange, 0))) {
          # <https://proj.org/operations/projections/merc.html>
          # <https://desktop.arcgis.com/en/arcmap/10.3/guide-books/
          #  map-projections/mercator.htm>
          return("merc")
        } else if (isTRUE(all.equal(.self$proj_lon0_lat0[2], 90.0))) {
          # <https://proj.org/operations/projections/stere.html>
          # <https://desktop.arcgis.com/en/arcmap/10.3/guide-books/
          #  map-projections/polar-stereographic.htm>
          return("stere")
        } else {
          # <https://proj.org/operations/projections/lcc.html>
          # <https://desktop.arcgis.com/en/arcmap/10.3/guide-books/
          #  map-projections/lambert-conformal-conic.htm>
          return("lcc")
        }
      }

      projection <- domainProjectionClass(
        name=auto_choose_projname(),
        lon0=.self$proj_lon0_lat0[1],
        lat0=.self$proj_lon0_lat0[2]
      )

      ###################
      # Initialise grid #
      ###################
      # (a) Get projected coords of grid center
      center_xy <- projection$lonlat2xy(
        lon=.self$center_lonlat[1],
        lat=.self$center_lonlat[2]
      )

      # (b) Grid x-axis
      x_range <- .self$ngrid_lonlat[1] * grid_spacing
      grid_xaxis <- gridAxisConfigClass(
        start=center_xy$x - 0.5 * x_range,
        end=center_xy$x + 0.5 * x_range,
        npts=.self$ngrid_lonlat[1],
        npts_ezone=.self$ezone_ngrid
      )

      # (c) Grid y-axis
      y_range <- ngrid_lonlat[2] * grid_spacing
      grid_yaxis <- gridAxisConfigClass(
        start=center_xy$y - 0.5 * y_range,
        end=center_xy$y + 0.5 * y_range,
        npts=ngrid_lonlat[2],
        npts_ezone=.self$ezone_ngrid
      )

      # (d) Set _grid attr
      .self$grid <- domainGridClass(
          xaxis=grid_xaxis,
          yaxis=grid_yaxis,
          proj=projection
      )
    }
  )
)
