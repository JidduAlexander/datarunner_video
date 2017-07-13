

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # DATABASE -----
  
  # Initialise databases
  rv_db <- reactiveValues(
    user         = db_user,
    user_info    = db_user_info,
    frame        = db_frame,
    frame_upload = db_frame_upload,
    # Modification times to check reloading
    user_mtime          = db_user_mtime,
    user_info_mtime     = db_user_info_mtime,
    frame_mtime         = db_frame_mtime,
    frame_upload_mtime  = db_frame_upload_mtime
  )
    
  # # Read the database
  # source("R/read_data_reactive.R", local = TRUE)
  # # Write the database
  # source("R/write_data_reactive.R", local = TRUE)
  
  # USER -----
  
  # User - like cookies
  rv_user <- reactiveValues(
    user_id   = NULL,
    user_name = NULL
  )
  
  # Message when logged in
  output$welcome_msg <- renderText({
    if(!is.null(rv_user$user_name)) {
      paste("You are logged in as ", rv_user$user_name)
    } else { NULL }
  })
  
  # UI for login in 
  output$login_ui <- renderUI({
    if(is.null(rv_user$user_id)) {
      list(
        hr(),
        textInput("user_name", "User name"),
        passwordInput("user_pw", "Password"),
        withBusyIndicatorUI(actionButton("user_login", "Login",
                                         class = "btn-primary")),
        div(style = "padding:10px;", p("or")),
        withBusyIndicatorUI(actionButton("create_account_1", "Create Account",
                                         class = "btn-primary"))
      )
    } else { NULL }
  })
  
  # UI for login out
  output$logout_ui <- renderUI({
    if(is.null(rv_user$user_id)) {
      NULL
    } else {
      list(
        hr(),
        div(style = "padding:10px;", textOutput("welcome_msg")),
        withBusyIndicatorUI(actionButton("user_logout", "Logout",
                                         class = "btn-primary"))
      )
    }
  })
  
  # If name or password is not filled disable the login button
  observe({
    if(!is.null(input$user_name)) {
    if(input$user_pw == "" | input$user_name == "") {
      disable("user_login")
    } else {
      enable("user_login")
    }
  }
  })
  
  # Trying to login
  observeEvent(input$user_login, {
    withBusyIndicatorServer("user_login", {
      
      user_ <- rv_db$user %>% 
        filter(name == input$user_name)
      if(nrow(user_) == 1) {
        # If the password is correct then login
        if(paste(as.character(hash(charToRaw(input$user_pw))), collapse = "") == user_$pw_hash) {
          # Set reactive values of currently logged in person
          rv_user$user_id   <- user_$user_id
          rv_user$user_name <- user_$name
          
          updateTabItems(session, "tab_profile", "profile_general")
          
        } else {
          # Reset pw field
          reset("user_name")
          reset("user_pw")
          stop("Wrong username password combination.")
        }
      } else {
        # Reset pw field
        reset("user_name")
        reset("user_pw")
        stop("Wrong username password combination.")
      }
    })
  })
  
  # Logout
  observeEvent(input$user_logout, {
    withBusyIndicatorServer("user_logout", {
      rv_user$user_id   <- NULL
      rv_user$user_name <- NULL
      
      Sys.sleep(0.5)
    })
  })
  
  # If create_account button is clicked go to create account tab
  observeEvent(input$create_account_1, {
    updateTabItems(session, "tab_main", "create_account")
  })
  observeEvent(input$create_account_2, {
    updateTabItems(session, "tab_main", "create_account")
  })
  
  # PAGE - HOME -----
  
  output$home_ui <- renderUI({
    ui <-  p("VIDEO: quick steps, large jumps, fly (whitch video), Sprint, backwards, leisure, Hinkelen, play backward slowly 120fps. 
             Questions: Does my running style have a speed limit. What's efficient running? 
             What leads to fewer injuries?
             Brief explanation of the App.")
    
    # Add login/create account if not logged in
    if(is.null(isolate(rv_user$user_id))) {
      ui <- c(
        ui,
        list(
          hr(),
          h3("Let's go!", style = "padding-bottom:20px;"),
          fluidRow(
            column(width = 5, p("Login in the side panel")),
            column(width = 1, p("or")),
            column(width = 6, withBusyIndicatorUI(actionButton("create_account_2", "Create Account",
                                                               class = "btn-primary")))
          )
        )
      )
    }
    div(
      style = "text-align:center; width:70%; max-width:600px; margin: 0 auto;",
      ui
    )
  })
  
  # PAGE - CREATE ACCOUNT -----
  
  # UI flipping through pages
  output$create_account_ui <- renderUI({
    ui <- NULL
    if(!is.null(isolate(rv_user$user_id))) {
      ui <- p("Please log out first.")
    } else {
      ui <- div(
        style = "width:70%; max-width:600px; margin: 0 auto;",
        div(
          style = "text-align:center;",
          h3("Login details"),
          fluidRow(
            column(width = 4, p(id = "input_text", "* User name")),
            column(width = 8, textInput("create_account_name", NULL, value = ""))
          ),
          fluidRow(
            column(width = 4, p(id = "input_text", "Email")),
            column(width = 8, textInput("create_account_email", NULL, value = ""))
          ),
          fluidRow(
            column(width = 4, p(id = "input_text", "* Password")),
            column(width = 8,  passwordInput("create_account_password1", NULL))
          ),
          fluidRow(
            column(width = 4, p(id = "input_text", "* Retype Password")),
            column(width = 8, passwordInput("create_account_password2", NULL))
          ),
          p("You are at at the forefront of running analysis curiousity. Your awesomeness 
            has the fortunate advantage that you can influence the future of this 
            application. We welcome your feedback and directions and we may approach you 
            asking for it."),
          hr(),
          h3("A little more about you"),
          p(paste0("Depending on you experience and body you probably have different 
                 strengths and weaknesses. To take this into the analysis please 
                 fill out the following form.")),
          p("GENERAL")
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "Year of birth")),
          column(width = 8, selectInput("runner_yob", NULL, 
                                        choices  = 1900:as.numeric(str_sub(date(), -4, -1)), 
                                        selected = "1990"))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "Height in cm")),
          column(width = 8, numericInput("runner_height", NULL, 
                                         min = 50, value = 175, step = 1))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "Weight in kg")),
          column(width = 8, numericInput("runner_weight", NULL, 
                                         min = 0, value = 65, step = 0.5))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "Sex")),
          column(width = 8, radioButtons("runner_sex", NULL, 
                                         choices  = c("Male", "Female", "Other"),
                                         selected = "Male"))
        ),
        div(style = "text-align:center;", p("INJURIES")),
        fluidRow(
          column(width = 4, p(id = "input_text", "What types of injuries do you deal with?")),
          column(width = 8, checkboxGroupInput("type_of_injuries", NULL, 
                                               choices  = types_of_injuries))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "How do injuries influence your running?")),
          column(width = 8, checkboxGroupInput("influence_of_injuries", NULL, 
                                               choices  = influences_of_injuries))
        ),
        div(style = "text-align:center;", p("RUNNING")),
        fluidRow(
          column(width = 4, p(id = "input_text", "What's your aim?")),
          column(width = 8, checkboxGroupInput("runner_aim", NULL,
                                               choices  = runner_aims))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "How long have you been running?")),
          column(width = 8, radioButtons("runner_experience", NULL,
                                         choices  = runner_experiences, 
                                         selected = runner_experiences[6]))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "How many runs (sessions) do you do per week?")),
          column(width = 8, numericInput("runner_runs_per_week", NULL, 
                                         min = 0, value = 3, step = 1))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "What is the average weekly distance (in km) that you run?")),
          column(width = 8, numericInput("runner_average_weekly_distance", NULL, 
                                         min = 0, value = 25, step = 1))
        ),
        fluidRow(
          column(width = 4, p(id = "input_text", "What types of training do you do?")),
          column(width = 8, checkboxGroupInput("runner_type_of_training", NULL, 
                                               choices  = runner_types_of_training))
        ),
        fluidRow(
          column(width = 4, 
                 p(id = "input_text", 
                   "I didn't (want to) fill out the form. (If you didn't actually fill the form we'd 
                   like to know it such that we know to ignore the data)")),
          column(width = 8, checkboxInput("input_form_correct", NULL, value = FALSE))
        ),
        div(style = "text-align:center;",
            withBusyIndicatorUI(
              actionButton("create_account_button", "Create Account",
                           style = "color: #232323; background-color: #65ff00; border-color: #434343;"))
        ),
        div(style = "height:200px")
      )
    }
    ui
  }) 
  
  observeEvent(input$create_account_button, {
    
    # Read the database
    source("R/read_data_reactive.R", local = TRUE)
    
    # Check if account name is entered
    validate(
      need(input$create_account_name != "", 
           "Please enter a user name.")
    )
    # Check if account name is unique
    validate(
      need(!(input$create_account_name %in% c(rv_db$user$name, rv_db$user$email)), 
           "User name is already taken, please enter another one.")
    )
    # If email is filled in, check if email address is valid
    if(input$create_account_email != "") {
      validate(
        need(isValidEmail(input$create_account_email), 
             "Invalid email address")
      )
    }
    # Check if email address is unique 
    validate(
      need(!(input$create_account_email %in% c(rv_db$user$email)), 
           "Email address already in use.")
    )
    # Check if password was filled in
    validate(
      need(input$create_account_password1 != "", 
           "Please enter a password")
    )
    # Check if passwords are the same
    validate(
      need(input$create_account_password1 == input$create_account_password2, 
           "Passwords not equal.")
    )
    
    # Add new user to user database
    new_user <- tibble(
      user_id     = max(rv_db$user$user_id) + 1,
      name        = input$create_account_name,
      email       = input$create_account_email,
      pw_hash     = input$create_account_password1 %>% 
        charToRaw() %>% 
        hash() %>% 
        as.character() %>% 
        paste(collapse = ""),
      date_joined = date()
    )
    
    rv_db$user <- bind_rows(rv_db$user, new_user)
    
    # Add new user info to user info database
    new_user_info <- tibble(
      user_id                        = new_user$user_id,
      name                           = new_user$name,
      runner_yob                     = as.numeric(input$runner_yob),
      runner_height                  = as.numeric(input$runner_height),
      runner_weight                  = as.numeric(input$runner_weight),
      runner_sex                     = as.character(input$runner_sex),
      # injuries
      injuries_ankle                 = types_of_injuries[1] %in% input$type_of_injuries,
      injuries_knee                  = types_of_injuries[2] %in% input$type_of_injuries,
      # influence_of_injuries
      deal_with_injuries_pain_run    = influences_of_injuries[1] %in% input$influence_of_injuries,
      deal_with_injuries_stop_run    = influences_of_injuries[2] %in% input$influence_of_injuries,
      deal_with_injuries_long_break  = influences_of_injuries[3] %in% input$influence_of_injuries,
      # aim
      aim_joy                        = runner_aims[1] %in% input$runner_aim,
      aim_health                     = runner_aims[2] %in% input$runner_aim,
      aim_amature                    = runner_aims[3] %in% input$runner_aim,
      aim_pro                        = runner_aims[4] %in% input$runner_aim,
      # runner amount/history
      runner_experience              = as.numeric(input$runner_experience),
      runner_runs_per_week           = as.numeric(input$runner_runs_per_week),
      runner_average_weekly_distance = as.numeric(input$runner_average_weekly_distance),
      # training types
      training_type_home_jog         = runner_types_of_training[1] %in% input$runner_type_of_training,
      training_type_interval_sprint  = runner_types_of_training[2] %in% input$runner_type_of_training,
      training_type_treadmill        = runner_types_of_training[3] %in% input$runner_type_of_training,
      # filled out correctly
      input_form_correct             = !input$input_form_correct
    )
    
    rv_db$user_info <- bind_rows(rv_db$user_info, new_user_info)
    
    # Write the database
    source("R/write_data_reactive.R", local = TRUE)
    
    # Login
    rv_user$user_id   <- new_user$user_id
    rv_user$user_name <- new_user$name
    
    # Go to page 2
    updateTabItems(session, "tab_profile", "profile_general")
  })
  
  # PAGE - PROFILE GENERAL -----
  
  output$profile_general_ui <- renderUI({
    ui <- NULL
    if(!is.null(rv_user$user_id)) {
      ui <- list(
        div(title = "New video analysis",
            style = "width:50%; float:left;",
                actionButton("new_video_analysis_button",
                             label = span(img(src="new_video_analysis.png", style = "width:100%;")))),
        div(title = "Load external data", 
            style = "width:50%; float:right;",
            actionButton("load_external_data",
                         label = span(img(src="matrix_data.png", style = "width:100%;"))))
      )
    }
    
    div(
      style = "text-align:center; width:80%; max-width:900px; margin: 0 auto;",
      ui
    )
  })
  
  # Go to tab when image is clicked
  observeEvent(input$new_video_analysis_button, {
    updateTabItems(session, "tab_profile", "profile_video_analysis")
  })
  
  # MENUS -----
  
  # main menu
  output$menu_main <- renderMenu({
    if(is.null(rv_user$user_id)) {
      sidebarMenu(
        id = "tab_main", 
        menuItem("Main",
                 menuSubItem("Home", tabName = "home"),
                 menuSubItem("Create Account", tabName = "create_account"))
      )
    } else { 
      sidebarMenu(
        id = "tab_main", 
        menuItem("Main",
                 menuSubItem("Home", tabName = "home"))
      )
    }
  })
  
  # profile menu
  output$menu_profile <- renderMenu({
    if(!is.null(rv_user$user_id)) {
      sidebarMenu(
        id = "tab_profile", 
        menuItem("Profile",
                 menuSubItem("General", tabName = "profile_general"),
                 menuSubItem("Runs", tabName = "profile_runs"),
                 menuSubItem("Video Analysis", tabName = "profile_video_analysis"))
      )
    } else { NULL }
  })
  
  # NEW VIDEO ANALYSIS (NVA) -----
  
  rv_nva <- reactiveValues(
    run_distance      = NULL, 
    run_pace_min_mile = NULL,
    run_pace_min_km   = NULL,
    run_pace_km_hr    = NULL,
    run_pace          = NULL,
    report_toggle     = NULL
  )
  
  # NVA: Run Info -----
  
  # Return pace ui when radioButton has been chosen
  output$run_pace_ui <- renderUI({
    if(length(input$run_pace) != 0) {
      # Toggle to enable button
      rv_nva$run_pace <- input$run_pace
      
      # return appropraite ui element
      if(input$run_pace == 'mm:ss per mile') {
        ui <- fluidRow(
          column(width = 3, numericInput("run_pace_1", NULL, value = 8, 
                                         min = 0, max = 59, step = 1)),
          column(width = 1, div(style = "float:left;", p("min"))),
          column(width = 3, numericInput("run_pace_2", NULL, value = 30, 
                                         min = 0, max = 59, step = 1)),
          column(width = 5, div(style = "float:left;", p("sec per mile")))
        )
      } else if(input$run_pace == 'mm:ss per km') {
        ui <- fluidRow(
          column(width = 3, numericInput("run_pace_1", NULL, value = 5, 
                                         min = 0, max = 59, step = 1)),
          column(width = 1, div(style = "float:left;", p("min"))),
          column(width = 3, numericInput("run_pace_2", NULL, value = 30, 
                                         min = 0, max = 59, step = 1)),
          column(width = 5, div(style = "float:left;", p("sec per km")))
        )
      } else if(input$run_pace == 'km per hour') {
        ui <- fluidRow(
          column(width = 3, numericInput("run_pace_1", NULL, min = 0, value = 12, step = 0.1)),
          column(width = 9, div(style = "float:left;", p("km per hour")))
        )
      }
      
      fluidRow(
          column(width = 4, p(id = "input_text", "My pace/speed is")),
          column(width = 8, ui)
      )
                 
    } else { NULL }
  })
  
  # Disable next button
  observe({
    if(is.null(rv_nva$run_pace)) {
      disable("video_analysis_next_1")
    } else {
      enable("video_analysis_next_1")
    }
  })
  
  observeEvent(input$video_analysis_next_1, {
    
    # Store data in reactivevalue till updated in database with other values in reporting step.
    
    # distance
    rv_nva$run_distance <- input$run_distance
    
    # pace
    if(input$run_pace == "mm:ss per mile") {
      run_pace_min_mile <- input$run_pace_1 + (input$run_pace_2 / 60)
      run_pace_min_km   <- run_pace_min_mile / 1.60934
      run_pace_km_hr    <- 60 / run_pace_min_km
    } else if(input$run_pace == "mm:ss per km") {
      run_pace_min_km   <- input$run_pace_1 + (input$run_pace_2 / 60)
      run_pace_min_mile <- run_pace_min_km * 1.60934
      run_pace_km_hr    <- 60 / run_pace_min_km
    } else if(input$run_pace == "km per hour") {
      run_pace_km_hr    <- input$run_pace_1
      run_pace_min_km   <- 60 / run_pace_km_hr
      run_pace_min_mile <- run_pace_min_km * 1.60934
    }
    
    rv_nva$run_pace_min_mile <- run_pace_min_mile
    rv_nva$run_pace_min_km   <- run_pace_min_km
    rv_nva$run_pace_km_hr    <- run_pace_km_hr
    
    updateTabItems(session, "tab_new_video_analysis", "Upload Video")
  })
  
  # NVA: VIDEO STUFF -----
  
  # Reactive video variables
  rv_vid <- reactiveValues(
    inFile          = NULL,
    temp_file       = NULL,
    frame_rate      = NULL,
    frame_rate_orig = NULL,
    frame_rate_mult = NULL,
    frames          = NULL,
    frame_start     = NULL,
    frame_end       = NULL,
    loaded          = FALSE
  )
  
  # Upload and read as data
  observeEvent(input$upload_video, {
    
    rv_vid$inFile <- input$upload_video
    
    withProgress(message = 'Preparing video', {
      
      # Temporary file name
      rv_vid$temp_file <- paste0("temp", gsub('\\D+', '', Sys.time()), ".mp4")
      
      # Set fontsize to video pixels
      fontsize <- system(paste('ffprobe -v error -of flat=s=_ -select_streams v:0 -show_entries',
                               ' stream=height,width', 
                               rv_vid$inFile$datapath), 
                         intern = TRUE)
      fontsize <- round((as.numeric(str_sub(fontsize[1], 24, -1)) + 
                           as.numeric(str_sub(fontsize[2], 25, -1))) / 50, 0)
      
      # Get frame rate
      frm_info1 <- system(paste('ffprobe -v error -select_streams v:0 -show_entries stream=avg_frame_rate,nb_frames',
                               ' -of default=noprint_wrappers=1:nokey=1', 
                               rv_vid$inFile$datapath), 
                         intern = TRUE)
      fr <- frm_info1[1] %>% 
        str_split(pattern = "/") %>% 
        unlist() %>% 
        as.numeric()
      rv_vid$frame_rate      <- 30
      rv_vid$frame_rate_orig <- fr[1]/fr[2]
      rv_vid$frame_rate_mult <- rv_vid$frame_rate_orig / rv_vid$frame_rate
      
      # Copy video and paste with frame numbers
      system(paste('ffmpeg -r 30 -i', rv_vid$inFile$datapath, 
                   '-ss ', input$upload_start_time * rv_vid$frame_rate_mult,
                   '-t', (input$upload_end_time - input$upload_start_time) * rv_vid$frame_rate_mult,
                   # Set font and text location
                   '-vf  "drawtext=fontfile=/Windows/Fonts/arial.ttf: x=(w-tw)/20: y=20: ',
                   paste('fontsize=', fontsize, ':'), # set fontsize
                   # Text n means write frame number (n) 
                   'fontcolor=white: box=1: boxcolor=0x00000000@1: text=%{n}:"',
                   '-an -y',
                   paste0("www/temp/", rv_vid$temp_file))) # Output file name
    })
    
    # Info of loaded video, because numeric end time may have gone beyond video time
    frm_info2 <- system(paste('ffprobe -v error -select_streams v:0 -show_entries stream=avg_frame_rate,nb_frames',
                             ' -of default=noprint_wrappers=1:nokey=1', 
                             paste0("www/temp/", rv_vid$temp_file)), 
                       intern = TRUE)
    rv_vid$frames          <- as.numeric(frm_info2[2])
    rv_vid$frame_start     <- round(input$upload_start_time * rv_vid$frame_rate_orig)
    rv_vid$frame_end       <- rv_vid$frame_start + rv_vid$frames - 1
    
    rv_vid$loaded <- TRUE
    
    # Go to next tab
    updateTabItems(session, "tab_new_video_analysis", "Select frames")
  })
  
  # Render video
  output$upload_show_video <- renderUI({
    rv_vid$inFile
    if(isolate(rv_vid$loaded)) {
      list(
        div(
          style = "width:80%; margin: 0 auto;",
          HTML(paste('<video id="v0" controls tabindex="0" width = "100%" max-height = "600px">',
                     '<source type="video/webm; codecs=&quot;vp8, vorbis&quot;" ',
                     paste0('src="temp/', rv_vid$temp_file, '">'),
                     '</source>'))
        ),
        div(
          style = "border:1px solid; border-top:none; width:80%; margin: 0 auto; padding:10px",
          fluidRow(
            column(width = 1),
            column(width = 3, h4("ADVANCED VIDEO CONTROLS")),
            # Forward and backward frame buttons
            column(width = 2,
                   div(style='float:left;', actionButton("back0", NULL, icon = icon("chevron-left"))), 
                   actionButton("forward0", NULL, icon = icon("chevron-right")),
                   p("Move 1 frame (not in Chrome)")),
            column(width = 6, 
                   sliderInput("video_slow", "Video play back rate",
                               min = 0.1, max = 10, value = 4, step = 0.1, ticks = FALSE,
                               label = div(style='width:300px;', 
                                           div(style='float:left;', 'fast'), 
                                           div(style='float:right;', 'slow'))))
          ),
          # Set input$vid0_currentTime to current video time when clicked
          tags$script("
                    document.getElementById('back0').onclick = function() {
                    var myVid = document.getElementById('v0');
                    Shiny.onInputChange('vid0_currentTime', myVid.currentTime);
                    };
                    document.getElementById('forward0').onclick = function() {
                    var myVid = document.getElementById('v0');
                    Shiny.onInputChange('vid0_currentTime', myVid.currentTime);
                    };
                    ")
        )
      )
    } else { NULL }
  })
  
  # Move one frame forwards or backwards
  observeEvent(input$back0, {
    js$vid0(input$vid0_currentTime - 1 / rv_vid$frame_rate)
  })
  observeEvent(input$forward0, {
    js$vid0(input$vid0_currentTime + 1 / rv_vid$frame_rate)
  })
  
  # Reduce/incerase video speed
  observeEvent(input$video_slow, {
    js$speed0(1 / input$video_slow)
  })
  
  # NVA: FRAME SELECTION -----
  
  rv_frame <- reactiveValues(
    selection         = NULL,
    frame_upload_id   = NULL,
    current_frame_num = NULL
  )
  
  output$video_frame_start_ui <- renderUI({
    if(!is.null(rv_vid$frame_end)) {
      selectInput("video_frame_start", NULL, 
                  choices  = rv_vid$frame_start:rv_vid$frame_end,
                  selected = rv_vid$frame_start)
    } else { NULL }
  })
  
  output$video_frame_end_ui <- renderUI({
    if(!is.null(rv_vid$frame_end)) {
      selectInput("video_frame_end", NULL, 
                  choices  = rv_vid$frame_start:rv_vid$frame_end,
                  selected = rv_vid$frame_end)
    } else { NULL }
  })
  
  # Frames to images
  observeEvent(input$video_analysis_next_2, {
    withBusyIndicatorServer("video_analysis_next_2", {
      withProgress(message = 'Preparing frame images', {
        
        video_frame_start <- as.numeric(input$video_frame_start)
        video_frame_end   <- as.numeric(input$video_frame_end)
        
        rv_frame$selection <- video_frame_start:video_frame_end
        
        # Video Loading parameters
        start <- (video_frame_start - rv_vid$frame_start) / rv_vid$frame_rate
        end   <- (1 + video_frame_end - rv_vid$frame_start) / rv_vid$frame_rate
        
        start_hr   <- as.character(start %/% 3600)
        start_min  <- as.character(start %% 3600 %/% 60)
        start_sec  <- as.character(start %% 3600 %% 60)
        
        end_hr   <- as.character(end %/% 3600)
        end_min  <- as.character(end %% 3600 %/% 60)
        end_sec  <- as.character(end %% 3600 %% 60)
        
        duration <- end - start
        
        seq_frames <- 1:(video_frame_end + 1 - video_frame_start)
        
        # Read the database
        source("R/read_data_reactive.R", local = TRUE)
        
        new_frame_upload_id <- max(rv_db$frame_upload$frame_upload_id) + 1
        
        # Reactive values for app response
        rv_frame$frame_upload_id   <- new_frame_upload_id
        rv_frame$current_frame_num <- rv_frame$selection[1]
        
        new_frame_upload <- tibble(
          frame_upload_id = new_frame_upload_id,
          user            = rv_user$user_id,
          date_upload     = date()
        )
        
        new_frame <- tibble(
          frame_id        = seq_frames + max(rv_db$frame$frame_id),
          frame_upload_id = new_frame_upload_id,
          frame_num       = rv_frame$selection,
          path            = paste0('img-u', rv_user$user_id, '-fu', new_frame_upload_id, "-",
                                   str_pad(as.character(seq_frames), width = 4, side = "left", pad = "0"), 
                                   ".png")
        )
        
        # Create single images
        system(paste0('ffmpeg -ss ', start_hr, ':',  start_min, ':', start_sec,
                      ' -i "www/temp/', rv_vid$temp_file, '" ',
                      '-t ', duration,
                      ' "www/frames/img-u', rv_user$user_id, '-fu', new_frame_upload_id, '-%04d.png"'))
        
        # Add new frames to database
        rv_db$frame        <- bind_rows(rv_db$frame, new_frame)
        rv_db$frame_upload <- bind_rows(rv_db$frame_upload, new_frame_upload)
        
        # Write the database
        source("R/write_data_reactive.R", local = TRUE)
        
        # Go to next tab
        updateTabItems(session, "tab_new_video_analysis", "Analyse frames")
      })
    })
  })
  
  # NVA: FRAME ANALYSIS -----
  
  output$frame_analysis_img <- renderUI({
    if(!is.null(rv_frame$selection)) {
      # Read the database
      source("R/read_data_reactive.R", local = TRUE)
      
      path <- isolate(rv_db$frame) %>% 
        dplyr::filter(frame_num == rv_frame$current_frame_num & frame_upload_id == rv_frame$frame_upload_id) %>% 
        dplyr::select(path)
      
      img(src= paste0("frames/", path[[1]]), align = "center", width = "100%", height = "auto")
    } else { NULL }
  })
  
  # Create next frame button / Next step at last frame
  output$next_frame_ui <- renderUI({
    if(!is.null(rv_frame$current_frame_num) & !is.null(rv_frame$selection)) {
      if(rv_frame$current_frame_num != max(rv_frame$selection)) {
        actionButton("next_frame", "Save and Next Frame", 
                     style = "color: #232323; background-color: #65ff00; border-color: #434343; ")
      } else {
        actionButton("next_frame", "Continue", 
                     style = "color: #232323; background-color: #65ff00; border-color: #434343; ")
      }
    } else { NULL }
  })
  
  observeEvent(input$next_frame, {

    # Retrieve radiobutton data
    row_data <- rep(NA, 8)
    row_data[c(1, 5)] <- c(input$left_air_ground, input$right_air_ground)
    if(input$left_air_ground == "Air") {
      row_data[2:4] <- NA_character_
    } else {
      row_data[2:3] <- c(input$left_ground_pos, input$left_ground_land)
      if(input$left_ground_land == "Landing") {
        row_data[4] <- input$left_ground_land_detail
      } else {
        row_data[4] <- NA_character_
      }
    } 
    if(input$right_air_ground == "Air") {
      row_data[6:8] <- NA_character_
    } else {
      row_data[6:7] <- c(input$right_ground_pos, input$right_ground_land)
      if(input$right_ground_land == "Landing") {
        row_data[8] <- input$right_ground_land_detail
      } else {
        row_data[8] <- NA_character_
      }
    } 
    
    # Read the database
    source("R/read_data_reactive.R", local = TRUE)
    
    # Change data
    rv_db$frame[rv_db$frame$frame_upload_id == rv_frame$frame_upload_id & 
                  rv_db$frame$frame_num == rv_frame$current_frame_num, # Row selection
                c("left_air_ground", "left_ground_pos", # Col selection
                  "left_ground_land", "left_ground_land_detail", 
                  "right_air_ground", "right_ground_pos", 
                  "right_ground_land", "right_ground_land_detail")] <- row_data
    
    # Write the database
    source("R/write_data_reactive.R", local = TRUE)
    
    # # Reset radiobuttons
    # reset("left_air_ground")
    # reset("left_ground_pos")
    # reset("left_ground_land")
    # reset("left_ground_land_detail")
    # reset("right_air_ground")
    # reset("right_ground_pos")
    # reset("right_ground_land")
    # reset("right_ground_land_detail")

    if(rv_frame$current_frame_num != max(rv_frame$selection)) {
      # Current frame +1
      rv_frame$current_frame_num <- rv_frame$current_frame_num + 1
    } else {
      # Maybe set all reactive value to NULL?
      rv_frame$current_frame_num <- rv_frame$selection[1]
      
      # Toggle to generate report
      rv_nva$report_toggle <- TRUE
    }
  })
  
  observeEvent(rv_nva$report_toggle, {
    if(!is.null(rv_nva$report_toggle)) {
      
      # Read the database
      source("R/read_data_reactive.R", local = TRUE)

      # Summarise frames data
      analysis <- rv_db$frame[rv_db$frame$frame_upload_id == rv_frame$frame_upload_id, ] %>% 
        mutate(
          step_left  = cumsum(c(1, if_else(left_air_ground == lag(left_air_ground), 0, 1)[-1])),
          step_right = cumsum(c(1, if_else(right_air_ground == lag(right_air_ground), 0, 1)[-1]))
        ) %>%
        summarise(
          num_frames         = n(),
          frames_air         = sum(left_air_ground == "Air" & right_air_ground == "Air", na.rm = TRUE),
          frames_ground      = num_frames - frames_air,
          frames_infront     = sum(left_ground_pos == "In front" | right_ground_pos == "In front", na.rm = TRUE),
          frames_behind      = sum(left_ground_pos == "Behind" | right_ground_pos == "Behind", na.rm = TRUE),
          frames_pos_flat    = sum(left_ground_land == "Flat" | right_ground_land == "Flat", na.rm = TRUE),
          frames_pos_land    = sum(left_ground_land == "Landing" | right_ground_land == "Landing", na.rm = TRUE),
          frames_pos_takeoff = sum(left_ground_land == "Taking off" | right_ground_land == "Taking off", na.rm = TRUE),
          frames_land_flat   = sum(left_ground_land_detail == "Flat" | right_ground_land_detail == "Flat", na.rm = TRUE),
          frames_land_heel   = sum(left_ground_land_detail == "Heel land" | right_ground_land_detail == "Heel land", na.rm = TRUE),
          frames_land_toe    = sum(left_ground_land_detail == "Toe land" | right_ground_land_detail == "Toe land", na.rm = TRUE),
          steps              = (max(step_left) + max(step_right) - 1) / 2
        ) %>%  
        mutate(
          air_ratio          = frames_air / num_frames,
          ground_ratio       = frames_ground / num_frames,
          front_behind_ratio = frames_infront / frames_behind,
          section_time       = (num_frames / rv_vid$frame_rate_orig),
          step_rate          = 60 * steps / section_time,
          step_length        = (1000 * rv_nva$run_pace_km_hr / 60) / step_rate
        ) %>% 
        select(-num_frames)
      
      rv_db$frame_upload[rv_db$frame_upload$frame_upload_id == rv_frame$frame_upload_id, 
                  c("distance", 
                    "pace_min_mile", "pace_min_km", "pace_km_hr", 
                    "frames_air", "frames_ground", 
                    "frames_infront", "frames_behind", 
                    "frames_pos_flat", "frames_pos_land", "frames_pos_takeoff", 
                    "frames_land_flat", "frames_land_heel", "frames_land_toe", 
                    "air_ratio", "ground_ratio", "front_behind_ratio", 
                    "steps", "section_time", "step_rate", "step_length")] <- list(
                      rv_nva$run_distance, 
                      rv_nva$run_pace_min_mile, rv_nva$run_pace_min_km, rv_nva$run_pace_km_hr,
                      analysis$frames_air, analysis$frames_ground, 
                      analysis$frames_infront, analysis$frames_behind, 
                      analysis$frames_pos_flat, analysis$frames_pos_land, analysis$frames_pos_takeoff, 
                      analysis$frames_land_flat, analysis$frames_land_heel, analysis$frames_land_toe, 
                      analysis$air_ratio, analysis$ground_ratio, analysis$front_behind_ratio, 
                      analysis$steps, analysis$section_time, analysis$step_rate, analysis$step_length
                    )
      
      # Write the database
      source("R/write_data_reactive.R", local = TRUE)
      
      # Go to next tab
      updateTabItems(session, "tab_new_video_analysis", "Report")
    }
  })
  
  # NVA: Report
  
  
  
}) # End server function
