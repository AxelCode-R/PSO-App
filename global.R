library(later)
library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(htmlwidgets)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(pagedown)
library(webshot2)
library(shinyjs)
library(RCurl)

library(lubridate)
library(rhandsontable)
library(data.table)
library(xts)
library(Matrix)

options(scipen=999)

for(src in list.files("R/")){
  source(paste0("R/", src))
}
for(src in list.files("shiny/")){
  source(paste0("shiny/", src))
}

#https://community.rstudio.com/t/how-to-properly-configure-google-chrome-on-shinyapps-io-because-of-webshot2/109020
