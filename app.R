# app script

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(RSocrata)
library(RcppRoll)
# library(leaflet)
# library(maps)
library(plotly)
# ibrary(ggforce)

# source all R files (including ui and server objects)
purrr::walk(list.files("R", ".*\\.R", full.names = TRUE), source)

shinyApp(ui, server)