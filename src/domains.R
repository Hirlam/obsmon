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
        c(.self$xmin, .self$ymax),
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
          c(.self$xmax, .self$ymax),
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
    nlat=function(...) .self$ny
  ),
  methods=list(
    lonlat2grid=function(lon, lat) {
      # Convert (lon, lat) into grid cell coord (i, j).
      xyDataframe <- .self$proj$lonlat2xy(lon, lat)
      return(.self$cart2grid(xyDataframe$x, xyDataframe$y))
    }
  )
)
