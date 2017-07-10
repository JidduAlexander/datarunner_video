library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)
library(shinyjs)
library(sodium)

source("R/helpers.R")
source("R/functions.R")


# IDEAS

# I just analysed my running facebook share button: Create funny compilation for sharing. 
# Give nicknames to running styles, like Micheal Air Jordan.


# Read databases

db_user         <- readRDS("input/db/db_user.Rds")
db_frame        <- readRDS("input/db/db_frame.Rds")
db_frame_upload <- readRDS("input/db/db_frame_upload.Rds")

# Get the modification times
db_user_mtime          <- file.mtime("input/db/db_user.Rds")
db_frame_mtime         <- file.mtime("input/db/db_frame.Rds")
db_frame_upload_mtime  <- file.mtime("input/db/db_frame_upload.Rds")

# Upload video 
# Select frames of two seconds of video
# For each frame say air/ground, before after hip

# Javascript code for video 
jsCode1 <- "shinyjs.speed0 = function(x){
  var myVid=document.getElementById('v0');
  myVid.playbackRate = x;
  $('#pbrate').html(x);
}"
jsCode2 <- "shinyjs.vid0 = function(value){document.getElementById('v0').currentTime = value;}"

# Upload limits
shiny_file_size <- 300 # in MB
video_footage <- 8 # In GB, much heavier in storage than video but frames can be specified

options(shiny.maxRequestSize = shiny_file_size * 1024^2)

# Remove the temporary video file from previous session, actually remove files that start with "temp"
fls <- list.files("www/temp")
if(length(fls[str_sub(fls, 1, 4) == "temp"]) != 0) {
  fls <- paste0("www/temp/", fls[str_sub(fls, 1, 4) == "temp"])
  file.remove(fls)
}

runner_experiences <- list(
  "Just starting" = 0,
  "1 Month"       = 1,
  "3 Months"      = 3,
  "6 Months"      = 6,
  "9 Months"      = 9,
  "1 Year"        = 12,
  "2 Years"       = 24,
  "3 Years"       = 36,
  "5+ Years"      = 60,
  "10+ Years"     = 120,
  "15+ Years"     = 180,
  "25+ Years"     = 300
)

runner_aims <-  c("I enjoy running",
                  "Staying healthy",
                  "Competitive (personal/amature)",
                  "Competitive (professional)")

runner_types_of_training <- c("Just leave my house and jog",
                              "Interval Trainging (sprints with breaks)",
                              "Treadmill")

run_distances <- c("Up to 200m", "A 400m sprint", "About 1km", "3 to 5km", "5 to 12km", "12 to 30km", "Marathon distance", "50km+")

types_of_injuries <- c("None", "Ankle", "Knee")

influences_of_injuries <- c("None", 
                            "Pain when running, but continue running",
                            "Stop (cut short) each run when I fell pain",
                            "Stop running for some time")

# Reusable chunks

# fluidRow(
#   column(width = 4, p(id = "input_text", "")),
#   column(width = 8)
# ),



