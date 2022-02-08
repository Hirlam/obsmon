##########################################################################
#       Observers/reactives related to domain grid & geometry            #
##########################################################################

fillInDomainDefaults <- function() {
  defaultsDefined <- DOMAIN$grid$hasPoints
  shinyjs::toggle("domainResetDefaults", condition=defaultsDefined)
  if(!defaultsDefined) return(NULL)
  updateNumericInput(session, "domainLonc", value=DOMAIN$center_lonlat[1])
  updateNumericInput(session, "domainLatc", value=DOMAIN$center_lonlat[2])
  updateNumericInput(session, "domainLon0", value=DOMAIN$proj_lon0_lat0[1])
  updateNumericInput(session, "domainLat0", value=DOMAIN$proj_lon0_lat0[2])
  updateNumericInput(session, "domainNlon", value=DOMAIN$ngrid_lonlat[1])
  updateNumericInput(session, "domainNlat", value=DOMAIN$ngrid_lonlat[2])
  updateNumericInput(session, "domainGridSpacing", value=DOMAIN$grid_spacing)
  updateMaterialSwitch(session, "domainLmrt", value=isTRUE(DOMAIN$lmrt))
}

fillInDomainDefaults()
observeEvent(input$domainResetDefaults, fillInDomainDefaults())

sessionDomainParams <- reactive(
  list(
    lonc=input$domainLonc,
    latc=input$domainLatc,
    lon0=input$domainLon0,
    lat0=input$domainLat0,
    lmrt=input$domainLmrt,
    nlon=input$domainNlon,
    nlat=input$domainNlat,
    gsize=input$domainGridSpacing
  )
) %>% debounce(500)

sessionDomain <- reactiveVal(initDomain(list(), stopOnError=FALSE))

domainInitialised <- reactiveVal(FALSE)
observeEvent(sessionDomainParams(), {
  req(isFALSE(domainInitialised()))
  sessionDomain(initDomain(sessionDomainParams(), stopOnError=FALSE))
  req(sessionDomain()$grid$hasPoints)
  domainInitialised(TRUE)
})

observeEvent({
  input$domainApplyChanges
  input$enableDomainUse
}, {

  if(input$enableDomainUse) {
    newDomain <- initDomain(sessionDomainParams(), stopOnError=FALSE)
  } else {
    newDomain <- EMPTY_DOMAIN
  }

  if(input$enableDomainUse && !newDomain$grid$hasPoints) {
    showNotification("Invalid domain configs", duration=1, type="error")
  }

  sessionDomain(newDomain)
}, ignoreInit=TRUE)

drawGridPts <- function(fig, grid, maxDisplayNLon=10, maxDisplayNLat=10) {
    # Add to fig a trace containing a selection of grid points.
    if(!grid$hasPoints || any(c(maxDisplayNLon, maxDisplayNLat) <= 0)) {
      # Just add an empty trace with mode="markers" in these cases,
      # to get rid of a "No scattergeo mode specifed" warning
      return(fig %>% add_trace(type="scattergeo",mode="markers"))
    }

    lonDrawEvery <- as.integer(max(1, round(grid$nx / maxDisplayNLon)))
    latDrawEvery <- as.integer(max(1, round(grid$ny / maxDisplayNLat)))

    name <- "Grid"
    nameAddendum <- c()
    if(lonDrawEvery > 1) {
      nameAddendum <- c(nameAddendum, sprintf("every %d lon", lonDrawEvery))
    }
    if(latDrawEvery > 1) {
      nameAddendum <- c(nameAddendum, sprintf("every %d lat", latDrawEvery))
    }
    if(length(nameAddendum) > 0) {
      name <- paste(name, "(") %>%
        paste0(paste(nameAddendum, collapse=", ")) %>%
        paste0(")")
    }

    ijGrid <- expand.grid(
      i=seq(1, grid$nx, lonDrawEvery),
      j=seq(1, grid$ny, latDrawEvery)
    )
    lonlat <- grid$grid2lonlat(i=ijGrid$i, j=ijGrid$j)

    fig <- fig %>%
      add_trace(type="scattergeo",
        name=name,
        lon=lonlat$lon,
        lat=lonlat$lat,
        mode="markers",
        marker=list(
          color="black",
          size=4
        )
      )

  return(fig)
}

output$modelDomainDemoChart <- renderPlotly({
  domain <- sessionDomain()

  rangeLon <- NULL
  rangeLat <- NULL
  map_center <- NULL
  if(domain$grid$hasPoints) {
     rangeLon <- c(domain$ezone_minlon - 1, domain$ezone_maxlon + 1)
     rangeLat <- c(domain$ezone_minlat - 1, domain$ezone_maxlat + 1)
     map_center <- list(lon=domain$center_lonlat[1], lat=domain$center_lonlat[2])
  }

  projParams <- domainProj2plotlyProj(domain=domain)

  plot_geo() %>%
    layout(
      title=ifelse(domain$grid$hasPoints,
        "Adopted Domain's Geometry and Grid",
        "Domain Geometry and Grid: None in Use"
      ),
      margin = list(
        t=100, # To leave space for the title
        b=10, # Looks better when figure is exported
        l=175, r=175 # To prevent legend from ending up too far away from plot
      ),
      showlegend = TRUE,
      legend = list(
        orientation="h", xanchor="center", x=0.5, yanchor="bottom", y=-0.05
      ),
      geo = list(
        # See <https://plot.ly/r/reference/#layout-geo>
        resolution = 50,
        showland = TRUE,
        showlakes = TRUE,
        showcountries=TRUE,
        landcolor = toRGB("grey98"),
        countrycolor = toRGB("grey50"),
        lakecolor = toRGB("white"),
        projection = list(
          type=projParams$name,
          parallels=projParams$params
        ),
        coastlinewidth = 0.5,
        countrywidth = 0.5,
        lataxis = list(
          range = rangeLat,
          showgrid = TRUE,
          dtick = 10
        ),
        lonaxis = list(
          range = rangeLon,
          showgrid = TRUE,
          dtick = 15
        ),
        center = map_center
      )
    ) %>%
    drawDomain(domain) %>%
    drawGridPts(domain$grid) %>%
    configPlotlyWrapper(
      edits=list(titleText=TRUE),
      toImageButtonOptions=list(filename="obsmon_domain")
    )
})
