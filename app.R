# app.R — Entry point
# 23MID0222 | BEPS 2001 British Election Panel Study
# CSI 3005 — Advanced Data Visualization Techniques | VIT

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)

# Load shared data, colours, and factor levels (sourced once here only)
source("global.R")

# Load UI layout
source("ui.R")

# Load server logic (scales_age helper defined at top of server.R)
source("server.R")

# Launch
shinyApp(ui = ui, server = server)