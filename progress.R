addTask <- function(t, name) UseMethod("addTask")
closeTracker <- function(t) UseMethod("closeTracker")
updateTask <- function(t, name, value) UseMethod("updateTask")

addTask.default <- function(t, name) {
  t$tasks[[name]] <- Progress$new()
  t$tasks[[name]]$set(message=name, value=0.)
  t
}

updateTask.default <- function(t, name, value) {
  t$tasks[[name]]$set(value=value)
  t
}

closeTracker.default <- function(t) {
  for (name in names(t$tasks)) {
    t$tasks[[name]]$close()
    t$tasks[[name]] <- NULL
  }
}

createShinyProgressTracker <- function() {
    t <- structure(list(), class = "shinyProgressTracker")
    t$tasks <- list()
    t
}
