
defaultMenuLabels <- list(
  experiment="Experiment",
  odbBase="Data Assimilation Category/Database",
  obtype="Observation Type",
  obname="Observation Name",
  sensor="Sensor",
  satellite="Satellite",
  channels="Channels",
  variable="Variable",
  levels="Levels",
  plottype="Type of Plot",
  station="Station"
)

getDefLabel <- function(inputId) {
  rtn <- defaultMenuLabels[[inputId]]
  if(is.null(rtn)) rtn <- inputId
  return(rtn)
}
