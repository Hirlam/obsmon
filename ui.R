jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

/* Initialise the app with the multiPlotsTab hidden */
shinyjs.init = function(){
  $('#appNavbarPage li a[data-value=\"multiPlotsTab\"]').hide();
}
"

appCSS <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}

#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}

.leaflet .legend {
  text-align: left;
}
"

shinyUI(ui=tagList(
  # Some definitions that apply to all tabs
  useShinyjs(),
  extendShinyjs(text=jscode, functions=c("disableTab", "enableTab", "init")),
  tags$script(inactivity), # Enable session timeout
  inlineCSS(appCSS),
  div(id="loading-content",h2(sprintf("Loading Obsmon v%s...",obsmonVersion))),
  # The page
  hidden(div(id="app-content",
    navbarPage(
      title=uiOutput("pageTitle"),
      windowTitle=ifelse(length(obsmonConfig$general$configName)>0,
        sprintf("Obsmon v%s %s", obsmonVersion, obsmonConfig$general$configName),
        paste0("Obsmon v", obsmonVersion)
      ),
      id="appNavbarPage",
      tabPanel("Main", value="mainTab", mainTab()),
      tabPanel(
        title="User-configured multiPlots",
        value="multiPlotsTab",
        multiPlotsTab()
      ),
      tabPanel("Domain Geometry & Grid", value="modelDomainTab", modelDomainTab())
    )
  ))
))
