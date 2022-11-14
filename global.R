library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(htmlwidgets)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)


for(src in list.files("R/")){
  source(paste0("R/", src))
}
for(src in list.files("shiny/")){
  source(paste0("shiny/", src))
}
