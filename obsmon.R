library(methods)
library(shiny)

shinydir <- Sys.getenv('PWD')
runApp(shinydir, launch.browser=FALSE, port=5391)
