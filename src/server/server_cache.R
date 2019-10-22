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

# Vars to establish/update queue of files that need to be cached
filesPendingCache <- reactiveVal(character(0))
filesPendingRecache <- reactiveVal(character(0))
newBatchFilesToCache <- reactiveVal(character(0))
newBatchFilesToRecache <- reactiveVal(character(0))
# Reset queues of files to cache/recache if db changes
# One could argue that it's fine to keep the queue going,
# but if the user changes dBs, they are probably no longer
# interested in the info present in the files on the queue,
# so not much point keep the processors working on this
activeDbChanged <- reactiveVal(0)
observeEvent(activeDb(), {
  filesPendingCache(character(0))
  filesPendingRecache(character(0))
  newBatchFilesToCache(character(0))
  newBatchFilesToRecache(character(0))
  activeDbChanged((activeDbChanged() + 1) %% 2)
})

# Get paths to data files associated with currently selected dB and DTG(s)
dataFilesForDbAndDtgs <- eventReactive({
  activeDbChanged()
  selectedDtgs()
}, {
  db <- req(activeDb())
  dtgs <- req(selectedDtgs())
  return(db$getDataFilePaths(dtgs))
})

# "asyncCachingProcs" keeps track of the ongoing processes for the caching
# of the various data files. These processes are "Future" objects (from R
# pkg "future").
asyncCachingProcs <- list()

# Establish/update queue of files that need to be cached
observeEvent(dataFilesForDbAndDtgs(), {
  newFiles <- dataFilesForDbAndDtgs()
  # Remove files for which caching is ongoing
  filesNotCachingNow <- newFiles[!(newFiles %in% names(asyncCachingProcs))]
  filesPendingCache(filesNotCachingNow)
})

# recacheRequested: To be used if the user manually requests recache or if
# obsmon detects that cache has finished but DTGs remain uncached
recacheRequested <- reactiveVal(FALSE)
# Establish/update queue of files that need to be recached (if requested)
observeEvent(recacheRequested(), {
  req(isTRUE(recacheRequested()))
  filesPendingRecache(unique(c(filesPendingRecache(),dataFilesForDbAndDtgs())))
  recacheRequested(FALSE)
})

# Keep track of caching activity
cacheIsOngoing <- reactiveVal(FALSE)

# Prepare and send batches of data files to be cached
observeEvent({
  filesPendingCache()
  cacheIsOngoing()
  }, {
  req(!isTRUE(cacheIsOngoing()))
  filesToCacheInThisBatch <- filesPendingCache()[1:2]
  filesToCacheInThisBatch <- Filter(Negate(anyNA), filesToCacheInThisBatch)
  newBatchFilesToCache(filesToCacheInThisBatch)
},
  # Give caching lower priority than other observers, as it will keep running
  # on the background for as long as needed and we don't want the app to slow
  # down as a result
  priority=-1
)
# Prepare and send, if requested, batches of data files to be re-cached
observeEvent({
  filesPendingRecache()
  cacheIsOngoing()
  }, {
  req(!isTRUE(cacheIsOngoing()))
  filesToRecacheInThisBatch <- filesPendingRecache()[1:2]
  filesToRecacheInThisBatch <- Filter(Negate(anyNA), filesToRecacheInThisBatch)
  newBatchFilesToRecache(filesToRecacheInThisBatch)
},
  priority=-1
)

# Cache (or recache) observations as new batches of file paths arrive
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

  cacheProc <- futureCall(
    FUN=putObsInCache,
    args=list(
      sourceDbPaths=fPaths,
      cacheDir=db$cacheDir,
      replaceExisting=isRecache
    )
  )
  # Register caching as "onging" for the relevant files
  cacheIsOngoing(TRUE)
  for(fPath in fPaths) asyncCachingProcs[[fPath]] <<- cacheProc

  then(cacheProc,
    onRejected=function(e) {flog.error(e)}
  )
  finally(cacheProc, function() {
    triggerReadCache()
    # Clean up entries from list of ongoing cache processes
    asyncCachingProcs[fPaths] <<- NULL
    if(isRecache) {
      recacheQueue <- filesPendingRecache()
      newRecacheQueue <- recacheQueue[!(recacheQueue %in% fPaths)]
      filesPendingRecache(newRecacheQueue)
    } else {
      cacheQueue <- filesPendingCache()
      newCacheQueue <- cacheQueue[!(cacheQueue %in% fPaths)]
      filesPendingCache(newCacheQueue)
    }
    cacheIsOngoing(FALSE)
  })

  # This NULL is necessary in order to prevent the future from blocking
  NULL
},
  priority=-1
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
    showModal(
      modalDialog("The experiment cache has been reset", easyClose=TRUE)
    )
  } else {
      signalError("Problems resetting experiment cache. Please check logs.")
  }
})

# Flagging that it's time to read info from cache
reloadInfoFromCache <- eventReactive({
    latestTriggerReadCache()
    activeDb()
    selectedDtgs()
  }, {
  Sys.time()
},
  ignoreNULL=FALSE
) %>% throttle(1000)

# Keep track of whether selected DTGs are cached or not
observeEvent(reloadInfoFromCache(), {
    selectedDtgsAreCached(dtgsAreCached(req(activeDb()),req(selectedDtgs())))
})

# Notify progress of caching
observeEvent({
  reloadInfoFromCache()
},{
  cacheNotifId="guiCacheNotif"
  if(isTRUE(selectedDtgsAreCached())) {
    removeNotification(cacheNotifId)
  } else {
    totalNFiles <- length(req(dataFilesForDbAndDtgs()))
    cacheProgressMsg <- tryCatch({
      nFilesPendingCache <- length(unique(c(
        filesPendingCache(), filesPendingRecache())
      ))
      nCachedFiles <- totalNFiles - nFilesPendingCache
      sprintf(
        "Caching selected DTGs: %d%%",
        round(100.0 * nCachedFiles / totalNFiles)
      )
    },
      warning=function(w) {return("Caching selected DTGs...")},
      error=function(e) {return("Caching selected DTGs...")}
    )
    showNotification(
      id=cacheNotifId, ui=cacheProgressMsg, type="message", duration=NULL
    )
  }
})
