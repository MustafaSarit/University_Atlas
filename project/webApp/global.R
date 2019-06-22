library(leaflet)
library(shiny)
library(shinyWidgets)

allDepartments <- read.csv("./departments.csv")
levels(allDepartments$Type)
allDepartments$X <- NULL
