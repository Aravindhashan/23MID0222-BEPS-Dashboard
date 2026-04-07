# global.R – Data loading, preprocessing, and shared globals
# CSI 3005 | Advanced Data Visualization Techniques | VIT

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(tidyr)

# ── Load BEPS dataset ──────────────────────────────────────────────────────────
beps <- read.csv("BEPS.csv", stringsAsFactors = FALSE)

# ── Preprocessing ──────────────────────────────────────────────────────────────
beps <- beps %>%
  mutate(
    vote = factor(vote, levels = c("Labour", "Conservative", "Liberal Democrat")),
    gender = factor(gender, levels = c("male", "female")),
    age_group = cut(
      age,
      breaks = c(17, 29, 49, 64, 100),
      labels = c("18-29", "30-49", "50-64", "65+")
    ),
    political.knowledge = factor(political.knowledge, levels = 0:3),
    economic.cond.national  = as.numeric(economic.cond.national),
    economic.cond.household = as.numeric(economic.cond.household),
    Blair   = as.numeric(Blair),
    Hague   = as.numeric(Hague),
    Kennedy = as.numeric(Kennedy),
    Europe  = as.numeric(Europe)
  ) %>%
  filter(!is.na(vote), !is.na(age), !is.na(gender))

# ── Canonical party colours ────────────────────────────────────────────────────
party_colors <- c(
  "Labour"           = "#E4002B",
  "Conservative"     = "#0033A0",
  "Liberal Democrat" = "#FF9F00"
)

# ── Age-group levels (used across server.R) ───────────────────────────────────
age_group_levels <- c("18-29", "30-49", "50-64", "65+")
