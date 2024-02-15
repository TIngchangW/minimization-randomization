require(shiny)
library(shiny)
library(tidyverse)
library(stringr)
library(glue)
folder_address = 'C:/Users/twang135/Study/JHU/biostatistician/research/shiny for minimization randomization/code'

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
z <- z[1]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip, ":1234/"))


app <- shinyApp(ui,server)
runApp(app, launch.browser=FALSE, port = 1234, host = ip)
