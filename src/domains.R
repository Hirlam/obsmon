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
    hasPoints=function(...) {
      return(isTRUE(
        length(.self$nx) > 0 &&
        length(.self$ny) > 0 &&
        all(c(.self$nx, .self$ny) > 0)
      ))
    },
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
    xy2grid=function(x, y) {
      # Return grid (i, j) cell containing projected (x, y) coords.
      data <- data.frame(x=x, y=y)
      data$i <- 1 + as.integer(signif((data$x - .self$xmin) / .self$x_spacing, 8))
      data$j <- 1 + as.integer(signif((data$y - .self$ymin) / .self$y_spacing, 8))
      return(subset(data, select=c(i, j)))
    },
    grid2xy=function(i, j) {
      data <- data.frame(i=i, j=j)
      data$x <- .self$xmin + (data$i - 1)*.self$x_spacing
      data$y <- .self$ymin + (data$j - 1)*.self$y_spacing
      return(subset(data, select=c(x, y)))
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

      # Handling case when the class is instantiated without any args
      if(length(lon)==1 && is.na(lon) && length(lat)==1 && is.na(lat)) {
        return(data.frame(x=NA, y=NA))
      }

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

      # Handling case when the class is instantiated without any args
      if(length(x)==1 && is.na(x) && length(y)==1 && is.na(y)) {
        return(data.frame(lon=NA, lat=NA))
      }

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
      xyDataframe <- .self$proj$lonlat2xy(lon=lon, lat=lat)
      return(.self$xy2grid(x=xyDataframe$x, y=xyDataframe$y))
    },
    grid2lonlat=function(i, j) {
      # Convert grid (i, j) into (lon, lat).
      xyDataframe <- .self$grid2xy(i=i, j=j)
      return(.self$proj$xy2lonlat(x=xyDataframe$x, y=xyDataframe$y))
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
      if(length(list(...)) == 0) return(NULL)

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
        ezone_ngrid=0
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
        if(isTRUE(.self$lmrt) || isTRUE(latrange > 35) || isTRUE(all.equal(latrange, 0))) {
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

initDomain <- function(config, stopOnError=TRUE) {
  if(length(config)==0) return(domainClass())

  missingAttrs <- c()
  for(attr in c("nlon", "nlat", "lon0", "lat0", "gsize")) {
    if(is.null(config[[attr]])) missingAttrs <- c(missingAttrs, attr)
  }
  if(length(missingAttrs) != 0) {
    msg <- "Missing domain attributes without defaults:"
    msg <- paste(msg, paste(missingAttrs, collapse=", "))
    if(stopOnError) {
      stop(msg)
    }
    else {
      flog.debug(msg)
      return(domainClass())
    }
  }

  setVal <- function(val, def) ifelse(is.null(val), def, val)
  lonc <- setVal(config$lonc, def=config$lon0)
  latc <- setVal(config$latc, def=config$lat0)

  domainParams <- list(
    name=setVal(config$name, def=""),
    center_lonlat=c(lonc, latc),
    proj_lon0_lat0=c(config$lon0, config$lat0),
    lmrt=setVal(config$lmrt, def=FALSE),
    ngrid_lonlat=c(config$nlon, config$nlat),
    grid_spacing=config$gsize,
    ezone_ngrid=setVal(config$ezone, def=0)
  )

  rtn <- tryCatch(
    do.call(domainClass, domainParams),
    error=function(e) {
      if(stopOnError) {
        stop(e)
      }
      else {
        flog.debug(e)
        return(domainClass())
      }
    }
  )
  return(rtn)
}

DOMAIN <- initDomain(obsmonConfig$domain)
EMPTY_DOMAIN <- initDomain(list(), stopOnError=FALSE)
lockBinding("DOMAIN", globalenv())
lockBinding("EMPTY_DOMAIN", globalenv())
