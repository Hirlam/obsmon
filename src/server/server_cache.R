##########################################################################
#                Caching-related observers/reactives                     #
##########################################################################
# Perform caching asyncronously as the user selects new DTGs and/or dBs.
# The reactive/observers defined here stablish a queue/schedule for the
# files to be cached. Once the user selects new DTGs/databases, the
# associated files are put in a queue to be cached. The files in that
# queue are sent to the caching routine in small batches, so that it is
# possible to reassign the order in which they should be parsed (e.g., to
# make sure the currently selected DTGs/dB have priority).

# Get paths to data files associated with currently selected dB and DTG(s)
dataFilesForDbAndDtgs <- eventReactive({
  activeDb()
  selectedDtgs()
}, {
  db <- req(activeDb())
  dtgs <- req(selectedDtgs())
  return(db$getDataFilePaths(dtgs))
})

# Initialise queue of files that need to be cached upon change of
# dataFilesForDbAndDtgs. The update of the queue while cache is ongoing
# is done in a observer defined further below.
filesPendingCache <- reactiveVal(character(0))
filesBeingCachedNow <- reactiveVal(character(0))
observeEvent(dataFilesForDbAndDtgs(), {
  newFiles <- dataFilesForDbAndDtgs()
  filesPendingCache(newFiles[!(newFiles %in% filesBeingCachedNow())])
})

# Initialise queue of files that have been requested to be recached
# The recaching itself is requested manually by the user by clicling
# on input$recacheCacheButton. An observer defined below will take
# care of triggering the recache.
filesPendingRecache <- reactiveVal(character(0))
recacheRequested <- reactiveVal(FALSE)
observeEvent(recacheRequested(), {
  req(isTRUE(recacheRequested()))
  filesPendingRecache(unique(c(filesPendingRecache(),dataFilesForDbAndDtgs())))
  recacheRequested(FALSE)
})

# Vars that will hold the batches of files to be sent for caching/recaching
# These "reactiveVal"s will be updated via an observer defined below. The
# reason for not defining them as reactives is that we'd like to give a lower
# priority to their updates w.r.t. the updates of other reactives (as caching
# will run in the background), but, at the moment, shiny does not allow
# setting the priority of reactives.
newBatchFilesToCache <- reactiveVal(character(0))
newBatchFilesToRecache <- reactiveVal(character(0))
observeEvent(dataFilesForDbAndDtgs(), {
  newBatchFilesToCache(character(0))
  newBatchFilesToRecache(character(0))
})

# Pause the caching engine if a plot or multiPlot is being performed
pauseCaching <- reactive({
  isTRUE(currentPlotPid()>-1) #|| isTRUE(multiPlotCurrentPid()>-1)
})

# Managing the queues of files to be cached/recached
observe({
  # Prepare and send batches of data files to be cached
  req(!cacheIsOngoing())
  if(isolate(pauseCaching())) invalidateLater(1000)
  req(isolate(!pauseCaching()))
  filesToCacheInThisBatch <- filesPendingCache()[1:2]
  filesToCacheInThisBatch <- Filter(Negate(anyNA), filesToCacheInThisBatch)
  isolate(newBatchFilesToCache(filesToCacheInThisBatch))
},
  # Lower priority: Caching will be running in the background; the other
  # processes are more important
  priority=-10
)
observe({
  # Prepare and send, if requested, batches of data files to be re-cached
  req(!isTRUE(cacheIsOngoing()))
  if(isolate(pauseCaching())) invalidateLater(1000)
  req(isolate(!pauseCaching()))
  filesToRecacheInThisBatch <- filesPendingRecache()[1:2]
  filesToRecacheInThisBatch <- Filter(Negate(anyNA), filesToRecacheInThisBatch)
  isolate(newBatchFilesToRecache(filesToRecacheInThisBatch))
},
  priority=-10
)

# Finally, cache (or recache) obs as new batches of file paths arrive
cacheProcPID <- reactiveVal(-1)
observeEvent(dataFilesForDbAndDtgs(), {
  # Stop caching if the user changes activeDb or DTGs,
  # so the relevant files for the new selection are cached instead.
  req(cacheProcPID() > 0)
  killProcessTree(cacheProcPID())
})
observeEvent({
  newBatchFilesToCache()
  newBatchFilesToRecache()
  }, {
  if(length(newBatchFilesToRecache())>0) {
    fPaths <- newBatchFilesToRecache()
    isRecache <- TRUE
  } else {
    fPaths <- newBatchFilesToCache()
    isRecache <- FALSE
  }
  req(length(fPaths)>0)
  db <- req(activeDb())

  # Register caching as "onging" for the relevant files
  cacheIsOngoing(TRUE)
  filesBeingCachedNow(fPaths)

  cacheProc <- futureCall(
    FUN=putObsInCache,
    args=list(
      sourceDbPaths=fPaths,
      cacheDir=db$cacheDir,
      replaceExisting=isRecache
    )
  )

  thisCacheProcPID <- cacheProc$job$pid
  cacheProcPID(thisCacheProcPID)
  session$onSessionEnded(function() {
    flog.debug(
      "Session finished: Making sure cache task with PID=%s is killed",
      thisCacheProcPID
    )
    killProcessTree(thisCacheProcPID)
  })

  then(cacheProc,
    onRejected=function(e) {
      # Make sure we don't leave incomplete cache entries behind
      flog.error(e)
      futureCall(
        FUN=rmObsFromCache,
        args=list(
          sourceDbPaths=fPaths,
          cacheDir=db$cacheDir
        )
      )
    }
  )
  finally(cacheProc, function() {
    if(isRecache) {
      recacheQueue <- filesPendingRecache()
      newRecacheQueue <- recacheQueue[!(recacheQueue %in% fPaths)]
      filesPendingRecache(newRecacheQueue)
    } else {
      cacheQueue <- filesPendingCache()
      newCacheQueue <- cacheQueue[!(cacheQueue %in% fPaths)]
      filesPendingCache(newCacheQueue)
    }
    killProcessTree(thisCacheProcPID)
    filesBeingCachedNow(NULL)
    cacheIsOngoing(FALSE)
    cacheProcPID(-1)
  })

  # This NULL is necessary in order to prevent the future from blocking
  NULL
},
  priority=-10
)

# Re-cache observations if requested by user
observeEvent(input$recacheCacheButton, {
  db <- req(activeDb())
  showNotification("Recaching selected DTG(s)", type="warning", duration=1)
  recacheRequested(TRUE)
},
  ignoreInit=TRUE
)

# Reset cache if requested by user
# Doing this in two steps to require confirmation
observeEvent(input$resetCacheButton, {
  showConfirmationDialog(
    inputId="resetCacheConfirmationButton",
    title="Are you sure?",
    msg=HTML(sprintf(paste(
        "Please confirm that you want to RESET all cached information ",
        "available for experiment %s%s%s",
        "This action cannot be undone!"
      ), "<br><br>", req(input$experiment), "<br><br>")
    )
  )
},
  ignoreInit=TRUE
)
observeEvent(input$resetCacheConfirmationButton, {
  status <- createCacheFiles(cacheDir=req(activeDb()$cacheDir), reset=TRUE)
  removeModal()
  if(status==0) {
    # Trigger a re-cache after erasing
    recacheRequested(TRUE)
    showModal(
      modalDialog("The experiment cache has been reset", easyClose=TRUE)
    )
  } else {
      signalError("Problems resetting experiment cache. Please check logs.")
  }
})

# Notify progress of caching
observe(
  shinyjs::toggleElement(
    selector=".caching_info_icon",
    anim=TRUE, animType="fade",
    condition=cacheIsOngoing() && !pauseCaching() && !selectedDtgsAreCached()
  )
)

observeEvent({
  filesPendingCache()
  cacheIsOngoing()
  reloadInfoFromCache()
  pauseCaching()
  recacheRequested()
},{
  cacheNotifId <- "guiCacheNotif"
  totalNFiles <- tryCatch(
    length(dataFilesForDbAndDtgs()),
    error=function(e) NULL
  )
  nFilesPendingCache <- tryCatch(
    length(unique(c(filesPendingCache(), filesPendingRecache()))),
    error=function(e) NULL
  )
  nCachedFiles <- tryCatch(
    totalNFiles - nFilesPendingCache,
    error=function(e) NULL
  )

  if (isTRUE(cacheIsOngoing())) {
    if (isTRUE(pauseCaching())) {
      showNotification(id=cacheNotifId, ui="Caching paused", duration=2)
    } else {
      cacheProgressMsg <- tryCatch(
        sprintf(
          "Caching selected DTGs: %d%%",
          round(100.0 * nCachedFiles / totalNFiles)
        ),
        error=function(e) {return("Caching selected DTGs...")}
      )
      showNotification(
        id=cacheNotifId, ui=cacheProgressMsg, type="message", duration=NULL
      )
    }
  }

  if(isTRUE(nCachedFiles==totalNFiles)) removeNotification(cacheNotifId)
}, ignoreNULL=FALSE, priority=-100)
