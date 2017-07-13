

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
              style = "width:70%; max-width:600px; margin: 0 auto;",
              div(style = "text-align:center;",
                  h3("About the run"),
                  p(paste0("You most likely have different running styles. You run a 400 meter different from a 
                       10km or a marathon. In the next step you will upload a video for analysis. We'd 
                       like to know what kind of run your video was for."))
              ),
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
                                    selected = character(0)))
              ),
              uiOutput("run_pace_ui"),
              div(style="margin: 0 auto; width:150px;",
                  withBusyIndicatorUI(
                    actionButton("video_analysis_next_1", "Next",
                                 style = "color: #232323; background-color: #65ff00; 
                                     border-color: #434343;"))
              )
            )
          ),
          tabPanel(
            # Upload Video -----
            title = "Upload Video",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              h3("Upload a video"),
              p("Upload a video of you running."),
              p(paste0("Your video may be long with unnecessary footage at the start and end. In that
                       case you can use start and end time to upload only a ",
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
              ),
              hr(style = "margin:80px 0px"),
              h3("Example video"),
              p("The numbers in the top left indicate frame (single images of the video) numbers 
                and are automatically added when you upload a video."),
              tags$video(src = "example_vid.mp4", type = "video/mp4", autoplay = NA, controls = NA,
                         style = "width:100%;")
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
              p("Here is the example video again.")
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
              style = "text-align:center; width:80%; max-width:600px; margin: 0 auto;",
              hr(style = "margin:80px 0px"),
              h3("Explanation with images"),
              p("The following is a sequence of frames for 1 step. Some frames have been omitted, 
                that means they have the same analysis result as the previous frame.")
            ),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis28.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(tags$li("Air")),
                     h5("Right Foot"),
                     tags$ul(tags$li("Air")),
                     p("Both feet are in the air, in frames 29 and 30 too.")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis31.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("In front"),
                       tags$li("Landing"),
                       tags$li("Heel land")
                     ),
                     p("The foot is getting in contact with the ground in front of the body. The 
                       toes are still in the air so it is a heel landing")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis32.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("In front"),
                       tags$li("Flat")
                     ),
                     p("The right foot is now completely flat on the ground. It is still in front 
                       of the body. Frames 33 and 34 will show the difference between in front and 
                       behind the body.")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis33r.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("In front"),
                       tags$li("Flat")
                     ),
                     p("The right foot is kind of getting underneath the body now. To have a clear 
                       decision we say that the foot is 'in front' if the heel is in front of the 
                       back of the bum, see the red line. In this case it is still in front.")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis34r.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("Behind"),
                       tags$li("Flat")
                     ),
                     p("The right foot has now gone behind the red line.")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis35.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("Behind"),
                       tags$li("Taking off")
                     ),
                     p("The heel is (very) slightly lifting if the ground. That means it is 
                       'taking off.' Frames 36, 37 and 38 are the same. ")
              ),
              column(width = 2)
            ),
            p(""),
            fluidRow(
              column(width = 2),
              column(width = 5, img(src='analysis38.png', width = "100%")),
              column(width = 3,
                     h5("Left Foot"),
                     tags$ul(
                       tags$li("Air")
                     ),
                     h5("Right Foot"),
                     tags$ul(
                       tags$li("Ground"),
                       tags$li("Behind"),
                       tags$li("Taking off")
                     ),
                     p("The toes are still in contact with the ground. That means it is still 'taking off'.
                       The next frame is like frame 28, but with the feet switched.")
              ),
              column(width = 2)
            )
          ),
          tabPanel(
            # Report -----
            title = "Report",
            div(
              style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
              h3("Here is your report"),
              p("Air percentage, Ground percentage, Landing, Step rate, Step length"),
              p("Plots comparing to other people."),
              p("Plot: Pace versus Air time coloured by distance."),
              p("Plot: Pace versus Step Rate coloured by distance."),
              p("Plot: Front behind ratio versus Air time coluored by distance."),
              p("Plot: Front behind ratio versus Step rate coluored by distance."),
              p("Plot: "),
              p("Plot: "),
              p("Plot: "),
              p("Plot: ")
            )
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
