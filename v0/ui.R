

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
            # Run Info -----
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
            # Upload Video -----
            title = "Upload Video",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              h3("Upload a video"),
              p("Upload a video of you running."),
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
          ),
          tabPanel(
            # Select Frames -----
            title = "Select frames",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              # INSPECT VIDEO -----
              h3("Inspect your video for desired frames"),
              p("Here we grab the frames (single images) from the video that we use for analysing 
              your running style."),
              p("Use the buttons and the slider to slow down the video and skip frames. Skipping 
              frames doesn't work in Chrome.")
            ),
            uiOutput("upload_show_video"),
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              # PICK START AND END FRAME -----
              h3("Pick start and end frame for the analysis"),
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
              withBusyIndicatorUI(
                actionButton("video_analysis_next_2", "Continue with selection",
                             style = "color: #232323; background-color: #65ff00; border-color: #434343;")     
              ),
              hr(style = "margin:80px 0px"),
              h3("Explanation by example"),
              p("Check out this example video")
            ),
            div(
              style = "width:70%; margin: 0 auto;",
              tags$video(src = "example_vid.mp4", type = "video/mp4", autoplay = NA, controls = NA,
                         style = "width:100%;")
            ),
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              p("The two images underneath show frame 28 and 61 of the example video. Frame 28 is the first frame 
                in the video where the back foot just lifted of the ground (see white circle). Frame 61 is the 
                last frame in the video where a foot (back foot) is still on the ground.")
            ),
            fluidRow(
              column(width = 6, img(src="frame28.png", style = "width:100%; padding:5px")),
              column(width = 6, img(src="frame61.png", style = "width:100%; padding:5px"))
            )
          ),
          tabPanel(
            # 5. ANALYSE FRAMES -----
            title = "Analyse frames",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              h3("Analyse, frame by frame"),
              p("For each frame we want to know for both the left and right foot whether
                       it is in the air or touching the ground. If it is touching the ground we 
                       also want to know:"),
              tags$ul(
                tags$li("if the foot is in front or behind you"), 
                tags$li("if the foot is landing, flat on the floor, or taking off"), 
                tags$li("if it is landing is it landing on the toe, on the heel or flat")
              ),
              p("Please see explanations with images underneath.")
            ),
            div(
              style = "border:1px solid #232323; width:80%; margin: 0 auto;",
              div(style = "width:100%;",
                  uiOutput("frame_analysis_img"),
                  p("")
              ),
              fluidRow(
                column(width = 1),
                column(
                  width = 5,
                  h4("LEFT FOOT", align = "center"),
                  fluidRow(
                    column(
                      width = 8, 
                      p(id = "input_text", "In the air or touching the ground")),
                    column(
                      width = 4, 
                      radioButtons("left_air_ground", NULL, 
                                   choices = c("Air", "Ground"),
                                   selected = "Air")
                    )
                  ),
                  conditionalPanel(
                    "input.left_air_ground == 'Ground'",
                    fluidRow(
                      column(
                        width = 8, 
                        p(id = "input_text", "Where is your foot with respect to center of mass (center of pelvic area)?")),
                      column(
                        width = 4, 
                        radioButtons("left_ground_pos", NULL, 
                                     choices = c("In front", "Behind"),
                                     selected = "In front")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8, 
                        p(id = "input_text", "Is the foot flat on the floor or landing?")),
                      column(
                        width = 4, 
                        radioButtons("left_ground_land", NULL, 
                                     choices = c("Flat", "Landing", "Taking off"),
                                     selected = "Flat"))
                    ),
                    conditionalPanel(
                      "input.left_ground_land == 'Landing'",
                      fluidRow(
                        column(
                          width = 8, 
                          p(id = "input_text", "How is your foot landing?")),
                        column(
                          width = 4, 
                          radioButtons("left_ground_land_detail", NULL, 
                                       choices = c("Flat", "Heel land", "Toe land"),
                                       selected = "Flat"))
                      )
                    )
                  )
                ),
                column(
                  width = 5,
                  h4("RIGHT FOOT", align = "center"),
                  fluidRow(
                    column(
                      width = 8, 
                      p(id = "input_text", "In the air or touching the ground")),
                    column(
                      width = 4, 
                      radioButtons("right_air_ground", NULL, 
                                   choices = c("Air", "Ground"),
                                   selected = "Air")
                    )
                  ),
                  conditionalPanel(
                    "input.right_air_ground == 'Ground'",
                    fluidRow(
                      column(
                        width = 8, 
                        p(id = "input_text", "Where is your foot with respect to center of mass (center of pelvic area)?")),
                      column(
                        width = 4, 
                        radioButtons("right_ground_pos", NULL, 
                                     choices = c("In front", "Behind"),
                                     selected = "In front")
                      )
                    ),
                    fluidRow(
                      column(
                        width = 8, 
                        p(id = "input_text", "Is the foot flat on the floor or landing?")),
                      column(
                        width = 4, 
                        radioButtons("right_ground_land", NULL, 
                                     choices = c("Flat", "Landing", "Taking off"),
                                     selected = "Flat"))
                    ),
                    conditionalPanel(
                      "input.right_ground_land == 'Landing'",
                      fluidRow(
                        column(
                          width = 8, 
                          p(id = "input_text", "How is your foot landing?")),
                        column(
                          width = 4, 
                          radioButtons("right_ground_land_detail", NULL, 
                                       
                                       choices = c("Flat", "Heel land", "Toe land"),
                                       selected = "Flat"))
                      )
                    )
                  )
                ),
                column(width = 1)
              ),
              div(style = "text-align:center;", uiOutput("next_frame_ui")),
              p("")
            ),
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              hr(style = "margin:80px 0px"),
              h3("Explanation with images"),
              h4("Air versus ground"),
              p("-"),
              h4("In front versus behind"),
              p("-"),
              h4("Flat versus Landing versus taking off"),
              p("-"),
              h4("Flat versus heel versus toe"),
              p("-")
            )
          ),
          tabPanel(
            # Report -----
            title = "Report"
          )
        ),
        div(style = "height:200px;"),
        p(" - ")
      )
    ),
    # Footer -----
    div(style = "width:100%; height:50px; background-color:#232323; position:fixed; bottom:0;
         right:0; color:#ececec; text-align:center;",
        div(style = "text-align:center; width:200px; float:left;",
            p("nothing here...")),
        div(style = "width:calc(100% - 200px); float:right;",
            p(" "),
            div(style = "text-align:center; width:33%; float:left;",
                p("Contact")),
            div(style = "text-align:center; width:33%; float:left;",
                p("Disclaimer")),
            div(style = "text-align:center; width:33%; float:right;",
                p("Links"))
        )
    )
  )
))
