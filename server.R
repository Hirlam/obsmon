# Keep track of how many sessions are connected
sessionsConnected <- reactiveVal(0)
# Stop app if all sessions are ended AND config file tells us to do so
# Keep it alive for "timeoutAfterAllSessionsEnd" seconds after all sessions
# closed, to prevent, for instance, killing app upon refresh of a lone session
nConnectedSessions <- reactive(sessionsConnected()) %>%
  debounce(1000*obsmonConfig$general$appTimeout)
observeEvent(nConnectedSessions(), {
  if(nConnectedSessions()<1) {
    flog.info(
      paste(
        "appTimeout: All sessions closed and none started within %ss.",
        "Stopping app."
      ),
      signif(obsmonConfig$general$appTimeout, 2)
    )
    stopApp()
  }
},
  ignoreInit=TRUE
)

# The server
shinyServer(function(input, output, session) {
  # Force garbage collection at the end of every session
  session$onSessionEnded(function() {gc()})
  # Log the session ID, as well as start end end times, to help when debugging
  sessionStartTime <- Sys.time()
  flog.info("New session started. Session token: %s", session$token)
  session$onSessionEnded(function() {
    flog.info(
      "Session %s ended after %.0fs.",
      session$token, as.numeric(Sys.time()-sessionStartTime, units="secs")
    )
  })

  # Increment global connected sessions count when session starts
  isolate(sessionsConnected(sessionsConnected() + 1))
  # Decrement global connected sessions count once session finishes
  session$onSessionEnded(function() {
    isolate({sessionsConnected(sessionsConnected() - 1)})
  })
  # Show, below the title, the number of currently connected sessions
  output$pageTitle <- renderUI({
    nSessions <- sessionsConnected()
    obsmonVersionText <- ifelse(length(obsmonConfig$general$configName)>0,
      sprintf("Obsmon v%s %s",obsmonVersion,obsmonConfig$general$configName),
      sprintf("Obsmon v%s", obsmonVersion)
    )
    if(nSessions>0) {
      sessionsConnectedText <- sprintf(
        '<p style="font-size:11px">#Sessions connected: %d</p>', nSessions
      )
      HTML(paste0(obsmonVersionText, sessionsConnectedText))
    } else {
      flog.debug("server.R: sessionsConnected()<1!")
      obsmonVersionText
    }
  })

  # Show code below the runApp is called with displayMode="showcase"
  # This happens if using "--debug" when running obsmon in standalone mode
  if(isTRUE(cmdLineArgs$debug)) shinyjs::runjs('toggleCodePosition();')

  # Loading info about configured experiments
  expts <- initExperiments()
  allExptNames <- unlist(lapply(expts, function(x) x$name))
  # Helper function to populate experiment choices in the GUI
  getNewExptChoices <- function(
    currentChoices=NULL, markAsAv=NULL, markAsUnav=NULL
  ) {
    unavExptsLabel <- "Experiments that could not be loaded"
    av <- NULL; unav <- NULL
    for(eName in allExptNames) {
      if(eName %in% markAsUnav) unav <- c(unav, eName)
      else if (eName %in% markAsAv) av <- c(av, eName)
      else if (eName %in% currentChoices[[unavExptsLabel]])unav<-c(unav,eName)
      else av <- c(av, eName)
    }
    if(length(av)==0) av <- c("ERROR: Could not load any experiment!"=" ")
    newChoices <- list(as.list(av), as.list(unav))
    names(newChoices) <- c(" ", unavExptsLabel)
    return(newChoices)
  }

  ####################################
  # Timeout-related server-side code #
  ####################################
  # Inform user if timeout is near
  timeoutTimerReset <- reactive(input$timeoutTimerReset) %>% throttle(500)
  nextTimeout <- eventReactive(timeoutTimerReset(), {
    Sys.time() + timeoutSeconds
  }, ignoreNULL=FALSE)
  observe({
    howLongToTimeout <- as.numeric(nextTimeout() - Sys.time(), units="secs")
    if(howLongToTimeout>timeoutWarnInSec || howLongToTimeout<0) {
      invalidateLater(1000 * (howLongToTimeout-timeoutWarnInSec), session)
      removeNotification(id="timeoutNearNotifID")
    } else {
      invalidateLater(500, session)
      timeoutWarnMsg <- sprintf(
        paste(
          "WARNING: The session will time out in %.0fs if it",
          "remains inactive! Move mouse and/or click somewhere to cancel."
        ),
        howLongToTimeout
      )
      showNotification(timeoutWarnMsg, type="error", id="timeoutNearNotifID")
    }
  })
  # Close session upon timeout. Code adapted from
  # <https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work>
  observeEvent(input$timeOut, {
    removeNotification(id="timeoutNearNotifID")
    sysTimeUTC <- strftime(Sys.time(),format="%Y-%m-%d %H:%M:%S %Z",tz="UTC")
    flog.warn(
      "Session %s timed out after %ss of inactivity",
      session$token, input$timeOut
    )
    showModal(modalDialog(
      title = "Timeout",
      sprintf(
        "Session timed out at %s after %ss of inactivity",
        sysTimeUTC, input$timeOut
      ),
      footer = NULL
    ))
    session$close()
  })

  # Attach a new list "userData$UiChoices" to the session, to keep track of the
  # menus' choices. It seems shiny doesn't have a native method for this.
  session$userData$UiChoices <- list()

  # Separating the logic for these sections, as they are quite distinct
  source("src/server/server_domain.R", local=TRUE)
  source("src/server/server_main_tab.R", local=TRUE)
  source("src/server/server_cache.R", local=TRUE)
  source("src/server/server_main_tab_plots.R", local=TRUE)
  source("src/server/server_multiplots.R", local=TRUE)
  source("src/server/server_doc_tab.R", local=TRUE)

  # Hide "Loading Obsmon" screen and show the app
  shinyjs::hide(id="loading-content", anim=FALSE)
  shinyjs::show("app-content")

}) # End of shinyServer
