# Keep track of how many sessions are connected
sessionsConnected <- reactiveVal(0)

# The server
shinyServer(function(input, output, session) {
  # Increment global connected sessions count when session starts
  isolate(sessionsConnected(sessionsConnected() + 1))
  # Decrement global connected sessions count once session finishes
  session$onSessionEnded(function() {
    isolate({sessionsConnected(sessionsConnected() - 1)})
  })
  # Show, below the title, the number of currently connected sessions
  output$pageTitle <- renderUI({
    nSessions <- sessionsConnected()
    obsmonVersionText <- sprintf("Obsmon v%s", obsmonVersion)
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

  # Write code info to the log for every session when using a shiny server
  # This info is already printed in a banner when running standalone
  if(!runningAsStandalone) cat(obsmonBanner)

  # Loading experiment data
  expts <- initExperiments()

  # Separating the logic for these sessions, as they are quite distinct 
  source("src/server/server_main_tab.R", local=TRUE)
  source("src/server/server_cache.R", local=TRUE)
  source("src/server/server_main_tab_plots.R", local=TRUE)
  source("src/server/server_multiplots.R", local=TRUE)
  source("src/server/server_doc_tab.R", local=TRUE)

}) # End of shinyServer
