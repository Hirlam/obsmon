fillObsmonDataFrameWithUnits <- function(
  df, varname=NULL, obname=NULL, varUnits=NULL, levelsUnits=NULL
) {
  if(is.null(varname)) varname <- df$varname
  varname <- unique(varname)

  if(is.null(obname)) obname <- df$obname
  obname <- unique(obname)

  obsvalueUnits <- NULL
  if(length(varname) == 1) obsvalueUnits <- getUnits(varname)

  for (colname in colnames(df)) {
    if(colname %in% .dataColsWithoutUnits) next
    if (colname == "level") {
      units(df[[colname]]) <- getUnitsForLevels(obname=obname, varname=varname)
      if(length(levelsUnits)>0 && levelsUnits != "") {
        tryCatch(
          units(df[[colname]]) <- levelsUnits,
          error=function(e) flog.error(e)
        )
      }
    } else if (colname %in% c("latitude", "longitude")) {
      units(df[[colname]]) <- getUnits("latlon")
    } else {
      tryCatch({
        units(df[[colname]]) <- obsvalueUnits
        if(length(varUnits)>0 && varUnits != "") {
          tryCatch(
            units(df[[colname]]) <- varUnits,
            error=function(e) flog.error(e)
          )
        }
      },
        error=function(e) {
          flog.error(
            "(fillObsmonDataFrameWithUnits): Can't set units for '%s': %s",
            colname, e
          )
        }
      )
    }
  }

  return(tibble(df))
}

getUnitsForLevels <- function(obname, varname=character(0)) {
  obstype <- getAttrFromMetadata("category", obname=obname)
  if(isTRUE(tolower(obname) == "satem")) return(NULL)
  levelType <- "pressure"
  if(obstype=="surface" || (isTRUE(obname=="radar") && !isTRUE(varname=="rh"))) {
    levelType <- "height"
  }
  return(getUnits(levelType))
}

getUnits <- function(quantity) {
  rtn <- .quantity2DefaultUnits[[quantity]]
  if(is.null(rtn)) {
    flog.warn("Could not determine units for quantity '%s'", quantity)
  }
  return(rtn)
}

.dataColsWithoutUnits <- c(
  "DTG",
  "obnumber",
  "obname",
  "satname",
  "varname",
  "passive",
  "nobs_total",
  "n_active_obs",
  "nobs_land",
  "nobs_sea",
  "statid",
  "active",
  "rejected",
  "passive",
  "blacklisted",
  "anflag",
  "status"
)

.quantity2DefaultUnits <- lapply(
  list(
    height = "m",
    apd  = "m",
    z    = "m",
    dbz  = "db",
    rh   = "%",
    rh2m = "%",
    t2m  = "K",
    t    = "K",
    td2m = "K",
    rad  = "K",
    bend_angle = "rad",
    pressure = "Pa",
    ps = "Pa",
    latlon = "degrees",
    u    = "m/s",
    ff   = "m/s",
    u10m = "m/s",
    ff10m= "m/s",
    v    = "m/s",
    v10m = "m/s",
    radv = "m/s",
    hlos = "m/s",
    snow = "kg/m2",
    q    = "kg/m3"
  ),
  function(string) {units(as_units(string))}
)
