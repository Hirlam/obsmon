suppressPackageStartupMessages(library(units))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(futile.logger))
flog.appender(appender.file(stderr()), 'ROOT')

fillObsmonDataFrameWithUnits <- function(
  df, varname=NULL, varUnits=NULL, levelsUnits=NULL
) {
  if(is.null(varname)) varname <- df$varname
  varname <- unique(varname)

  obsvalueUnits <- NULL
  if(length(varname) == 1) obsvalueUnits <- .getUnits(varname)
  print(paste(varname, obsvalueUnits)) # TEST

  for (colname in colnames(df)) {
    if(colname %in% .dataColsWithoutUnits) next
    if (colname == "level") {
      units(df[[colname]]) <- .getUnits("pressure")
      if(length(levelsUnits)>0 && levelsUnits != "") {
        tryCatch(
          units(df[[colname]]) <- levelsUnits,
          error=function(e) flog.error(e)
        )
      }
    } else if (colname %in% c("latitude", "longitude")) {
      units(df[[colname]]) <- .getUnits("coordinate_angles")
    } else {
      units(df[[colname]]) <- obsvalueUnits
      if(length(varUnits)>0 && varUnits != "") {
        tryCatch(
          units(df[[colname]]) <- varUnits,
          error=function(e) flog.error(e)
        )
      }
    }
  }

  return(tibble(df))
}

.getUnits <- function(quantity) {
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
  "nobs_land",
  "nobs_sea",
  "statid",
  "active",
  "rejected",
  "passive",
  "blacklisted",
  "anflag"
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
    coordinate_angles = "degrees",
    u    = "m/s",
    ff   = "m/s",
    u10m = "m/s",
    ff10m= "m/s",
    v    = "m/s",
    v10m = "m/s",
    radv = "m/s",
    snow = "kg/m2",
    q    = "kg/m3"
  ),
  function(string) {units(as_units(string))}
)
