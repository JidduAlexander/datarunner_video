

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  skin = "green",
  
  dashboardHeader(title = span(img(src="datarunner.png", width = "100%"))),
  
  dashboardSidebar(
    sidebarMenuOutput("menu_main"),
    sidebarMenuOutput("menu_profile"),
    uiOutput("login_ui"),
    uiOutput("logout_ui")
  ),
  
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode1),
    extendShinyjs(text = jsCode2),
    tags$head(tags$style(HTML('
      #input_text {
        float:right;
      }
    '))),
    tabItems(
      tabItem(
        tabName = "home",
        uiOutput("home_ui")
      ),
      tabItem(
        tabName = "create_account",
        uiOutput("create_account_ui")
      ),
      tabItem(tabName = "profile_general",
              uiOutput("profile_general_ui")
      ),
      tabItem(
        tabName = "profile_video_analysis",
        tabBox(
          id = "tab_new_video_analysis",
          width = 12,
          tabPanel(
            title = "Run info",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              h3("About the run"),
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
                      column(width = 3, numericInput("run_pace_1", NULL, min = 0, value = 12, step = 0.1)),
                      column(width = 9, div(style = "float:left;", p("km per hour")))
                    )
                  )
                )
              ),
              withBusyIndicatorUI(
                actionButton("video_analysis_next_1", "Next",
                             style = "color: #232323; background-color: #65ff00; border-color: #434343;"))
            )
          ),
          tabPanel(
            title = "Upload Video",
            fluidRow(
              column(width = 2),
              column(width = 8, 
                     h3("Upload a video"),
                     p("Upload a video where you run past"),
                     p(paste0("If your video is long you can use start and end time to upload only a ",
                       "segment of your video. Maximum video upload size is ", shiny_file_size, "MB.")),
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
              )
            )
          ),
          tabPanel(
            title = "Select frames",
            # INSPECT VIDEO -----
            h4("Inspect your video for desired frames"),
            p("Here we grab the frames (single images) from the video that we use for analysing 
              your running style."),
            p("Use the buttons and the slider to slow down the video and skip frames. Skipping 
              frames doesn't work in Chrome."),
            uiOutput("upload_show_video"),
            # PICK START AND END FRAME -----
            h4("3C. Pick first ans last frame of the analysis segment"),
            fluidRow(
              column(width = 4, 
                     p(id = "input_text", 
                       "Start frame (Look for the first frame where your back foot has lift of the ground)")),
              column(width = 8, uiOutput("video_frame_start_ui"))
            ),
            fluidRow(
              column(width = 4, 
                     p(id = "input_text", 
                       "End frame (Look for the last frame where your back foot is till on the ground)")),
              column(width = 8, uiOutput("video_frame_end_ui"))
            ),
            fluidRow(
              column(width = 4),
              column(width = 8, 
                     withBusyIndicatorUI(actionButton("video_frame_button", "Extract frame from video >")))
            ),
            p("The example underneath is a split of two frames. The left side shows the start frame 
                         (frame 28) and the left side shows the end frame (frame 61)."),
            fluidRow(column(width = 1),
                     column(width = 10, img(src="feetcontact.png", align = "center")),
                     column(width = 1)
            ),
            p(""),
            withBusyIndicatorUI(
              actionButton("video_analysis_next_2", "Next",
                           style = "color: #232323; background-color: #65ff00; border-color: #434343;")     
            )
          ),
          tabPanel(
            title = "Analyse frames",
            # 5. ANALYSE FRAMES -----
            fluidRow(
              column(width = 2),
              column(width = 8,
                     h4("4. Analyse, frame by frame"),
                     p("For each frame you have to answer a few questions.")
              ),
              column(width = 2)      
            ),
            fluidRow(
              column(width = 1),
              column(width = 10, uiOutput("frame_analysis_img")),
              column(width = 1)
            ),
            p(""),
            fluidRow(
              column(width = 1),
              column(width = 4,
                     h4("LEFT FOOT", align = "center"),
                     fluidRow(
                       column(width = 8, 
                              p(id = "input_text", "In the air or touching the ground")),
                       column(width = 4, 
                              radioButtons("left_air_ground", NULL, 
                                           choices = c("Air", "Ground"),
                                           selected = "Air")
                       )
                     ),
                     conditionalPanel(
                       "input.left_air_ground == 'Ground'",
                       fluidRow(
                         column(width = 8, 
                                p(id = "input_text", "Where is your foot with respect to center of mass (center of pelvic area)?")),
                         column(width = 4, radioButtons("left_ground_pos", NULL, 
                                                        choices = c("In front", "Behind"),
                                                        selected = "In front")
                         )
                       ),
                       fluidRow(
                         column(width = 8, p(id = "input_text", "Is the foot flat on the floor or landing?")),
                         column(width = 4, radioButtons("left_ground_land", NULL, 
                                                        choices = c("Flat", "Landing", "Taking off"),
                                                        selected = "Flat"))
                       ),
                       conditionalPanel(
                         "input.left_ground_land == 'Landing'",
                         fluidRow(
                           column(width = 8, p(id = "input_text", "How is your foot landing?")),
                           column(width = 4, radioButtons("left_ground_land_detail", NULL, 
                                                          choices = c("Flat", "Heel land", "Toe land"),
                                                          selected = "Flat"))
                         )
                       )
                     )
              ),
              column(width = 4,
                     h4("RIGHT FOOT", align = "center"),
                     fluidRow(
                       column(width = 8, 
                              p(id = "input_text", "In the air or touching the ground")),
                       column(width = 4, 
                              radioButtons("right_air_ground", NULL, 
                                           choices = c("Air", "Ground"),
                                           selected = "Air")
                       )
                     ),
                     conditionalPanel(
                       "input.right_air_ground == 'Ground'",
                       fluidRow(
                         column(width = 8, 
                                p(id = "input_text", "Where is your foot with respect to center of mass (center of pelvic area)?")),
                         column(width = 4, radioButtons("right_ground_pos", NULL, 
                                                        choices = c("In front", "Behind"),
                                                        selected = "In front")
                         )
                       ),
                       fluidRow(
                         column(width = 8, p(id = "input_text", "Is the foot flat on the floor or landing?")),
                         column(width = 4, radioButtons("right_ground_land", NULL, 
                                                        choices = c("Flat", "Landing"),
                                                        selected = "Flat"))
                       ),
                       conditionalPanel(
                         "input.right_ground_land == 'Landing'",
                         fluidRow(
                           column(width = 8, p(id = "input_text", "How is your foot landing?")),
                           column(width = 4, radioButtons("right_ground_land_detail", NULL, 
                                                          
                                                          choices = c("Flat", "Heel land", "Toe land"),
                                                          selected = "Flat"))
                         )
                       )
                     ),
                     div(style = "float:right;", uiOutput("next_frame_ui"))
              ),
              column(width = 1)
            )
          ),
          tabPanel(
            title = "Report"
          )
        )
      )
    ),
    # Footer
    div(style = "width:100%; height:200px;")
  )
))
