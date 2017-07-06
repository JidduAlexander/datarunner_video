

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode1),
  extendShinyjs(text = jsCode2),
  
  tags$head(tags$style(
    HTML('
         #input_text {
         float:right;
         }
         ')
  )),
  
  # Application title
  titlePanel("Data Runner"),
  
  # 1. General -----
  fluidRow(
    column(width = 2),
    column(
      width = 8, 
      h3("1. About you - General"),
      p(paste0("Depending on you experience and body you probably have different 
                    strengths and weaknesses. To take this into the analysis please 
                    fill out the following form.")),
      p("General"),
      fluidRow(
        column(width = 4, p(id = "input_text", "Age")),
        column(width = 8, numericInput("runner_age", NULL, 
                                       min = 1, value = 30, step = 1))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Height in cm")),
        column(width = 8, numericInput("runner_height", NULL, 
                                       min = 0, value = 175, step = 1))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Weight in kg")),
        column(width = 8, numericInput("runner_weight", NULL, 
                                       min = 0, value = 65, step = 0.5))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Sex")),
        column(width = 8, selectInput("runner_sex", NULL, 
                                      choices  = c("Male", "Female", "Other"),
                                      selected = "Male"))
      ),
      p("Running"),
      fluidRow(
        column(width = 4, p(id = "input_text", "Aim")),
        column(width = 8, selectInput("runner_aim", NULL,
                                      choices  = runner_aims, 
                                      selected = runner_aims[1],
                                      multiple = TRUE))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Experience")),
        column(width = 8, selectInput("runner_experience", NULL,
                                      choices  = runner_experiences, 
                                      selected = runner_experiences[6]))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Runs (sessions) per week")),
        column(width = 8, numericInput("runner_runs_per_week", NULL, 
                                       min = 0, value = 3, step = 1))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Average weekly distance in km")),
        column(width = 8, numericInput("runner_average_weekly_distance", NULL, 
                                       min = 0, value = 25, step = 1))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Types of training")),
        column(width = 8, selectizeInput("runner_type_of_training", NULL, 
                                         choices  = runner_types_of_training,
                                         multiple = TRUE))
      ),
      p("Injuries"),
      fluidRow(
        column(width = 4, p(id = "input_text", "Types of injuries")),
        column(width = 8, selectizeInput("type_of_injuries", NULL, 
                                         choices  = types_of_injuries,
                                         multiple = TRUE))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "Influences on running")),
        column(width = 8, selectizeInput("influence_of_injuries", NULL, 
                                         choices  = influences_of_injuries,
                                         multiple = TRUE))
      )
    ),
    column(width = 2)
  ),
  # 2. THE RUN ------
  fluidRow(
    column(width = 2),
    column(
      width = 8, 
      h3("2. The run we are going to analyse"),
      p(paste0("You most likely have different running styles. You run a 400 meter different from a 
                    10km or a marathon. In the next step you will upload a video for analysis. We'd 
                    like to know what kind of run your video was for.")),
      fluidRow(
        column(width = 4, p(id = "input_text", "The way I run in the video is how I run")),
        column(width = 8, selectInput("run_distance", NULL,
                                      choices  = run_distances, 
                                      selected = run_distances[5]))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "I know my pace/speed in")),
        column(width = 8, 
               radioButtons("run_pace", NULL,
                            choices  = c("mm:ss per mile", "mm:ss per km", "km per hour"),
                            selected = "mm:ss per km"))
      ),
      fluidRow(
        column(width = 4, p(id = "input_text", "My pace/speed is")),
        column(
          width = 8, 
          conditionalPanel(
            "input.run_pace == 'mm:ss per mile'",
            fluidRow(
              column(width = 3, numericInput("run_pace_1", NULL, value = 8, 
                                             min = 0, max = 59, step = 1)),
              column(width = 1, div(style = "float:left;", p("min"))),
              column(width = 3, numericInput("run_pace_2", NULL, value = 30, 
                                             min = 0, max = 59, step = 1)),
              column(width = 5, div(style = "float:left;", p("sec per mile")))
            )
          ),
          conditionalPanel(
            "input.run_pace == 'mm:ss per km'", 
            fluidRow(
              column(width = 3, numericInput("run_pace_1", NULL, value = 5, 
                                             min = 0, max = 59, step = 1)),
              column(width = 1, div(style = "float:left;", p("min"))),
              column(width = 3, numericInput("run_pace_2", NULL, value = 30, 
                                             min = 0, max = 59, step = 1)),
              column(width = 5, div(style = "float:left;", p("sec per km")))
            )
          ),
          conditionalPanel(
            "input.run_pace == 'km per hour'",
            fluidRow(
              column(width = 6, numericInput("run_pace_1", NULL, min = 0, value = 12, step = 0.1)),
              column(width = 6, div(style = "float:left;", p("km per hour")))
            )
          )
        )
      )
    ),
    column(width = 2)
  ),
  # 3. UPLOAD VIDEO -----
  fluidRow(
    column(width = 2),
    column(width = 8, 
           h3("3. Upload a video and select frames"),
           p("Here we grab the frames (single images) from the video that we use for analysing 
             your running style."),
           h4("3A. Upload video (segment)"),
           p(paste0("Here you can upload a video. Set the start and end time in seconds to ",
                    "roughly cut the part of the video you want to analyse. The size of the ",
                    "uploaded video file can't exceed ", shiny_file_size, "MB.")),
           fluidRow(
             column(width = 4, p(id = "input_text", "Start time (seconds)")),
             column(width = 8, numericInput("upload_start_time", NULL, value = 0, min = 0))
           ),
           fluidRow(
             column(width = 4, p(id = "input_text", "End time (seconds)")),
             column(width = 8, numericInput("upload_end_time", NULL, value = 30, min = 0))
           ),
           fluidRow(
             column(width = 4),
             column(width = 8, fileInput("upload_video", "Upload Video"))
           )
    ),
    column(width = 2)
  ),
  hr(),
  fluidRow(
    column(width = 2),
    column(width = 8, 
           h4("3B. Inspect your video"),
           p("Use the buttons and the slider to slow down the video and skip frames. Skipping 
             frames doesn't in Chrome."),
           uiOutput("upload_show_video")),
    column(width = 2)      
  ),
  fluidRow(
    column(width = 2),
    column(width = 8,
           h4("3C. Pick first ans last frame of the analysis segment"),
           fluidRow(
             column(width = 4, 
                    p(id = "input_text", 
                      "Start frame (Look for the first frame where your back foot has lift of the ground)")),
             column(width = 8, numericInput("video_frame_start", NULL, value = 0, min = 0))
           ),
           fluidRow(
             column(width = 4, 
                    p(id = "input_text", 
                      "End frame (Look for the last frame where your back foot is till on the ground)")),
             column(width = 8, numericInput("video_frame_end", NULL, value = 1, min = 0))
           ),
           p("The example underneath is a split of two frames. The left side shows the start frame 
             (frame 28) and the left side shows the end frame (frame 61)."),
           img(src="feetcontact.png", align = "right"),
           p("")
    ),
    column(width = 2)
  )
  # 5. FRAMES -----
))
  