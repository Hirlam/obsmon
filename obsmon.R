library(methods)
library(shiny)

shinydir<-Sys.getenv('PWD')
shinydir
launch.browser=TRUE
runApp(shinydir,launch.browser=TRUE)
