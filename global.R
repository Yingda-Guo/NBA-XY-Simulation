# Global.R
rm(list = ls())
library(shinydashboard)
library(jsonlite)
library(rjson)
library(data.table)
library(stringr)
library(DT)
library(plotly)
library(FNN)
library(shinyjs)
library(flexdashboard)
library(stringr)

library(cicerone)

# Create guide
guide <- Cicerone$
  new()$ 
  step(
    el = "box_plot",
    title = "Dynamic Animation",
    description = "After loading, click play button to start"
  )$
  step(
    "box_metrics",
    "Advanced Metrics",
    "Accel & Decel & Total Distance ect"
  )$
  step(
    "box_data",
    "Real-time data",
    "Raw x-y coordinate data"
  )$
  step(
    "box_event",
    "In-Game Event Prediction",
    "Using KNN algorithm to predict in game event, such as who posses ball & passing and shooting event"
  )

# Load the data for processing
source("_functions.R")

# Data preprocessing <- this will only run once for all sessions
# All files are stored in data folder
current_dir <- "data"

# Load data from othe sources
dt_event <- fread(paste0(current_dir,"/", 'raw_data.csv'))
dt_event[, name := paste0(firstname, " ", lastname)]

# Consider to change it to player name
player_ids <- dt_event[,unique(player_id, na.rm=T)]
player_ids <- player_ids[player_ids != "-1"]

player_names <- dt_event[, unique(name, na.rm = T)]
player_names <- player_names[!str_detect(player_names, "NA ball")]

# Load metric dictionary
metric_dic <- fread("data/metric_dic.csv")

