##########################################################################
#                Caching-related observers/reactives                     #
##########################################################################
# Perform caching assyncronously as the user selects new DTGs and/or dBs.
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

# Get paths to data files associated with currently selected DTG(s) and
# and dB, and which are not currently being cached
dataFilesForDbAndDtgs <- eventReactive({
  activeDbChanged()
  selectedDtgs()
}, {
  return(getFilePathsToCache(req(activeDb()), req(selectedDtgs())))
})

# "assyncCachingProcs" keeps track of the ongoing processes for the caching
# of the various data files. These processes are "Future" objects (from R
# pkg "future").
assyncCachingProcs <- list()

# Establish/update queue of files that need to be cached
observeEvent(dataFilesForDbAndDtgs(), {
  newFiles <- dataFilesForDbAndDtgs()
  # Remove files for which caching is ongoing
  filesNotCachingNow <- newFiles[!(newFiles %in% names(assyncCachingProcs))]
  filesPendingCache(unique(c(filesNotCachingNow, filesPendingCache())))
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
})
# Prepare and send, if requested, batches of data files to be re-cached
observeEvent({
  filesPendingRecache()
  cacheIsOngoing()
  }, {
  req(!isTRUE(cacheIsOngoing()))
  filesToRecacheInThisBatch <- filesPendingRecache()[1:2]
  filesToRecacheInThisBatch <- Filter(Negate(anyNA), filesToRecacheInThisBatch)
  newBatchFilesToRecache(filesToRecacheInThisBatch)
})

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
    req(!selectedDtgsAreCached())
  }
  req(length(fPaths)>0)
  db <- req(activeDb())

  cacheProc <- suppressWarnings(futureCall(
    FUN=putObsInCache,
    args=list(
      sourceDbPaths=fPaths,
      cacheDir=db$cacheDir,
      replaceExisting=isRecache
    )
  ))
  # Register caching as "onging" for the relevant files
  cacheIsOngoing(TRUE)
  for(fPath in fPaths) assyncCachingProcs[[fPath]] <<- cacheProc

  then(cacheProc,
    onRejected=function(e) {flog.error(e)}
  )
  finally(cacheProc, function() {
    triggerReadCache()
    # Clean up entries from list of ongoing cache processes
    assyncCachingProcs[fPaths] <<- NULL
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

  # This NULL is necessary in order to avoid the future from blocking
  NULL
})

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
  ignoreNULL=TRUE
) %>% throttle(2000)

# Keep track of whether selected DTGs are cached or not
observeEvent(reloadInfoFromCache(), {
    selectedDtgsAreCached(dtgsAreCached(req(activeDb()),req(selectedDtgs())))
})

# Periodically attempt to cache DTGs if they remain uncached even
# after the processes responsible for caching them have finished.
# This is useful to retry caching if former attempts fail
observeEvent({if(!selectedDtgsAreCached()) invalidateLater(10000)}, {
  req(!selectedDtgsAreCached())
  req(!cacheIsOngoing())
  showNotification("Attempting to recache", type="warning", duration=1)
  recacheRequested(TRUE)
},
  ignoreInit=TRUE
)
