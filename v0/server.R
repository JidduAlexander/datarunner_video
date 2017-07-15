

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # DATABASE -----
  
  # Initialise databases
  rv_db <- reactiveValues(
    user         = db_user,
    user_info    = db_user_info,
    injury       = db_injury,
    frame        = db_frame,
    frame_upload = db_frame_upload,
    # Modification times to check reloading
    user_mtime         = db_user_mtime,
    user_info_mtime    = db_user_info_mtime,
    injury_mtime       = db_injury_mtime,
    frame_mtime        = db_frame_mtime,
    frame_upload_mtime = db_frame_upload_mtime
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
            column(width = 6, p("Login in the side panel")),
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
  
  # Store injuries if more are created
  rv_create_account <- reactiveValues(
    injuries      = NULL,
    fill_form     = NULL, # support for actionbutton disable
    error_account = NULL, # support for actionbutton disable
    error_general = NULL, # support for actionbutton disable
    error_running = NULL, # support for actionbutton disable
    error_other   = NULL  # support for actionbutton disable
  )
  
  # CREATE ACCOUNT: DISABLE ACTION BUTTON -----
  
  # Update when radiobutton is clicked
  observeEvent(input$input_form_correct, {
    rv_create_account$fill_form <- input$input_form_correct
  })
  
  # Disable add account button
  observe({
    if(!is.null(input$create_account_name)) {
      if(is.null(rv_create_account$fill_form) | 
         !is.null(rv_create_account$error_account) | 
         !is.null(rv_create_account$error_general) | 
         !is.null(rv_create_account$error_running) | 
         !is.null(rv_create_account$error_other)){
        disable("create_account_button")
      } else {
        enable("create_account_button")
      }
    }
  })
  
  # CREATE ACCOUNT: MAIN UI PAGE -----
  
  # UI for the page 
  output$create_account_ui <- renderUI({
    ui <- NULL
    if(!is.null(isolate(rv_user$user_id))) {
      ui <- p("Please log out first.")
    } else {
      ui <- list(
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
          max-width:700px; margin:10px auto; padding:15px;",
          div(style = "text-align:center;", h3("Account details")),
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
          div(style = "text-align:center;color:#771111;", textOutput("error_account_msg"))
        ),
        div(
          style = "width:70%; max-width:600px; margin: 0 auto; text-align:center;",
          p("You are at at the forefront of running analysis curiousity. Your awesomeness 
            has the fortunate advantage that you can influence the future of this 
            application. We welcome your feedback and directions and we may approach you 
            asking for it."),
          hr(),
          h3("A little more about you"),
          p(paste0("Depending on you experience and body you probably have different 
                 strengths and weaknesses. To take this into the analysis please 
                 fill out the following form."))
        ), 
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
                  max-width:700px; margin:10px auto; padding:15px;",
          div(style = "text-align:center;", h3("General")),
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
          div(style = "text-align:center;color:#771111;", textOutput("error_general_msg"))
        ),
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
          max-width:700px; margin:10px auto; padding:15px;",
          div(style = "text-align:center;", h3("Running")),
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
          div(style = "text-align:center;color:#771111;", textOutput("error_running_msg"))
        ),
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
          max-width:700px; margin:10px auto; padding:15px;",
          div(style = "text-align:center;", h3("Other training")),
          fluidRow(
            column(width = 4, 
                   p(id = "input_text", "How often per week do you do strength training (gym) ?")),
            column(width = 8, numericInput("training_gym", NULL, 
                                           value = 0, min = 0, step = 1))
          ),
          fluidRow(
            column(width = 4, 
                   p(id = "input_text", "How often per week do you do cross fit training?")),
            column(width = 8, numericInput("training_crossfit", NULL, 
                                           value = 0, min = 0, step = 1))
          ),
          fluidRow(
            column(width = 4, 
                   p(id = "input_text", "How many minutes do you stretch before every run?")),
            column(width = 8, numericInput("training_stretch_before", NULL, 
                                           value = 0, min = 0, step = 1))
          ),
          fluidRow(
            column(width = 4, 
                   p(id = "input_text", "How many minutes do you stretch after every run?")),
            column(width = 8, numericInput("training_stretch_after", NULL, 
                                           value = 0, min = 0, step = 1))
          ),
          div(style = "text-align:center;color:#771111;", textOutput("error_other_msg"))
        ),
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
          max-width:700px; margin:10px auto; padding:15px;",
          div(style = "text-align:center;", h3("Injuries")),
          uiOutput("add_injuries_ui")
        ),
        div(
          style = "background-color:white; border:solid 1px #232323; width:80%; 
          max-width:700px; margin:20px auto; padding:15px; text-align:center;",
          div(style = "font-weight:bold;", p("Did you fill the complete form?")),
          radioButtons("input_form_correct", NULL, 
                       inline   = TRUE,
                       choices  = c("Yes" = TRUE, "No" = FALSE),
                       selected = character(0)),
          p("If you didn't actually fill the form we'd like to know it such that 
            we know to ignore the data in your analysis"),
          div(style = "text-align:center;color:#771111;", textOutput("error_actionbutton_msg")),
          withBusyIndicatorUI(
            actionButton("create_account_button", "Create Account",
                         style = "color: #232323; background-color: #65ff00; border-color: #434343;"))
        ),
        div(style = "height:200px")
      )
    }
    ui
  }) 
  
  # CREATE ACCOUNT: INJURY UI -----
  
  # Render the injuries part seperate to add more injuries
  output$add_injuries_ui <- renderUI({
    list(
      div(style = "text-align:center;", 
          p("Use the selection inputs and button to add injuries.")),
      div(
        style = "border:solid 1px grey; margin: 3px; padding: 5px; background-color:#e4f8ff;",
        fluidRow(
          column(width = 4, selectInput("injury_name", "Injury", 
                                        choices  = injury_name,
                                        selected = injury_name[1])),
          column(width = 2, selectInput("injury_run_type", "Comes up in", 
                                        choices  = c("Both", "Sprint", "Jog"), 
                                        selected = "Both")),
          column(width = 6, selectInput("injury_affect", "How does it affect you?",
                                        choices  = influences_of_injuries,
                                        selected = influences_of_injuries[1]))
        ),
        div(style = "text-align:right;", actionButton("injury_add_more", "Add injury"))
      ),
      
      div(style = "text-align:center;", 
          h4("Your injuries:"),
          fluidRow(
            column(width = 4, p(style = "font-weight:bold;", "Injury")),
            column(width = 2, p(style = "font-weight:bold;", "During")),
            column(width = 6, p(style = "font-weight:bold;", "Affect"))
          ),
          if(!is.null(rv_create_account$injuries)) {
            pmap(list(rv_create_account$injuries$name,
                      rv_create_account$injuries$run_type,
                      rv_create_account$injuries$affect),
                 function(x, y, z) {
                   fluidRow(
                     column(width = 4, p(paste0(x))),
                     column(width = 2, p(paste0(y))),
                     column(width = 6, p(paste0(z)))
                   )
                 })
          } else {
            div(style = "color:#111177;", p("You haven't added any injuries."))
          }
      )
    )
  })
  
  # Action button add injury response
  observeEvent(input$injury_add_more, {
    rv_create_account$injuries <- bind_rows(
      rv_create_account$injuries,
      tibble(
        name     = input$injury_name,
        run_type = input$injury_run_type,
        affect   = input$injury_affect
      )
    )
  })
  
  # CREATE ACCOUNT: ERROR MESSAGES -----
  
  # Check if inputs are valid and create error message if necessary
  output$error_account_msg <- renderText({
    text <- NULL
    if(input$create_account_name == "") {
      text <- paste(text, "Please enter a user name", sep = " - ")
    } else {
      if(input$create_account_password1 == "") {
        text <- paste(text, "Please enter a password", sep = " - ")
      }
    }
    if(input$create_account_name %in% c(rv_db$user$name, rv_db$user$email)) {
      text <- paste(text, "User name is already taken, please enter another one", sep = " - ")
    }
    if(input$create_account_email != "" & !isValidEmail(input$create_account_email)) {
      text <- paste(text, "Invalid email address", sep = " - ")
    }
    if(input$create_account_email %in% c(rv_db$user$email)) {
      text <- paste(text, "Email address already in use", sep = " - ")
    }
    if(input$create_account_password1 != input$create_account_password2) {
      text <- paste(text, "Passwords not equal", sep = " - ")
    }
    if(!is.null(text)) text <- str_sub(text, 4, -1)
    rv_create_account$error_account <- text
    text
  })
  
  # Check if inputs are valid and create error message if necessary
  output$error_general_msg <- renderText({
    text <- NULL
    if(!is.numeric(input$runner_height)) {
      text <- paste(text, "Make sure height is a number", sep = " - ")
    }
    if(!is.numeric(input$runner_weight)) {
      text <- paste(text, "Make sure weight is a number", sep = " - ")
    }
    rv_create_account$error_general <- text
    text
  })
  
  # Check if inputs are valid and create error message if necessary
  output$error_running_msg <- renderText({
    text <- NULL
    if(!is.numeric(input$runner_runs_per_week)) {
      text <- paste(text, "Make sure runs per week is a number", sep = " - ")
    }
    if(!is.numeric(input$runner_average_weekly_distance)) {
      text <- paste(text, "Make sure average weekly distance is a number", sep = " - ")
    }
    rv_create_account$error_general <- text
    text
  })
  
  # Check if inputs are valid and create error message if necessary
  output$error_other_msg <- renderText({
    text <- NULL
    if(!is.numeric(input$training_gym)) {
      text <- paste(text, "Make sure the strength input is a number", sep = " - ")
    }
    if(!is.numeric(input$training_crossfit)) {
      text <- paste(text, "Make sure the crossfit nput is a number", sep = " - ")
    }
    if(!is.numeric(input$training_stretch_before) | !is.numeric(input$training_stretch_after)) {
      text <- paste(text, "Make sure stretches inputs are numbers", sep = " - ")
    }
    rv_create_account$error_general <- text
    text
  })

  # Check if inputs are valid and create error message if necessary
  output$error_actionbutton_msg <- renderText({
    if(!is.null(rv_create_account$error_account) | 
       !is.null(rv_create_account$error_general) | 
       !is.null(rv_create_account$error_running) | 
       !is.null(rv_create_account$error_other)){
      "Please see fix any (error) message above."
    } else {
      NULL
    }
  })
  
  # CREATE ACCOUNT: CREATE THE ACCOUNT -----
  
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
      runner_bmi                     = runner_weight / (runner_height / 100)^2,
      runner_sex                     = as.character(input$runner_sex),
      # # injuries
      # injuries_ankle                 = types_of_injuries[1] %in% input$type_of_injuries,
      # injuries_knee                  = types_of_injuries[2] %in% input$type_of_injuries,
      # # influence_of_injuries
      # deal_with_injuries_pain_run    = influences_of_injuries[1] %in% input$influence_of_injuries,
      # deal_with_injuries_stop_run    = influences_of_injuries[2] %in% input$influence_of_injuries,
      # deal_with_injuries_long_break  = influences_of_injuries[3] %in% input$influence_of_injuries,
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
      # Other training
      training_gym                   = input$training_gym,
      training_crossfit              = input$training_crossfit,
      training_stretch_before        = input$training_stretch_before,
      training_stretch_after         = input$training_stretch_after,
      # filled out correctly
      input_form_correct             = if_else(input$input_form_correct == "FALSE", FALSE, TRUE)
    )
    
    rv_db$user_info <- bind_rows(rv_db$user_info, new_user_info)

    # Add injuries to injury database
    if(!is.null(rv_create_account$injuries)) {
      new_injury <- tibble(
        user_id  = new_user$user_id,
        name     = rv_create_account$injuries$name,
        run_type = rv_create_account$injuries$run_type,
        affect   = rv_create_account$injuries$affect
      )
      
      rv_db$injury <- bind_rows(rv_db$injury, new_injury)
    }
    
    # Write the database
    source("R/write_data_reactive.R", local = TRUE)
    
    # Reset injuries
    rv_create_account$injuries <- NULL
    
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
        div(
          style = "text-align:center; width:70%; margin: 0 auto;",
          h3("Would you like to do something data-like?"),
          p("With the video analysis you can compare your running style to everybody else's. This 
          is about building a large database of running styles and how they are related to 
          performance and injury."),
          p("Loading external data is not yet a feature. But the idea is that it allows a much more 
          personalised and advanced analysis compared to what external parties offer.")
        ),
        div(
          style = "text-align:center; width:80%; max-width:900px; margin: 0 auto;",
          div(title = "New video analysis",
              style = "width:50%; float:left;",
              actionButton("new_video_analysis_button",
                           label = span(img(src="new_video_analysis.png", style = "width:100%;")))),
          div(title = "Load external data", 
              style = "width:50%; float:right;",
              actionButton("load_external_data",
                           label = span(img(src="matrix_data.png", style = "width:100%;")))),
          # p("(tabs can be found in the side panel)")
          p("-")
        )
      )
      
      frame_uploads <- filter(rv_db$frame_upload, user_id == rv_user$user_id)
      if(length(frame_uploads) != 0) {
        
        output$frame_uploads_table <- renderDataTable({
          transmute(frame_uploads, 
                    Distance              = distance, 
                    `Pace (mm:ss per km)` = paste0(
                      str_pad(pace_min_km %/% 1, width = 2, side = "left", pad = "0"), ":",
                      str_pad( round(pace_min_km %% 1 * 60), width = 2, side = "left", pad = "0")),
                    `Speed (km/hr)`       = pace_km_hr,
                    `Air time`            = paste0(round(air_ratio * 100), "%"),
                    `Behind time`         = paste0(round(behind_ratio * 100), "%"),
                    `Step Rate`           = round(step_rate))
        })
        
        ui <- c(
          ui, 
          list(div(
            style = "text-align:center; width:70%; margin: auto;",
            hr(style = "margin:20px 0px"),
            h3("A summary table of your video analyses"),
            p("The following table contains one row of information for each video analysis you did."),
            div(
              style = "background-color:white; margin:20px 0px;",
              dataTableOutput("frame_uploads_table")
            ),
            hr(style = "margin:40px 0px"),
            h3("Interactive plotting environment"),
            p("Use the controls two filter and plot what you like. You are totally free to perform your 
              own amazing analysis."),
            p("Controls: x, y, colour, facet x, facet y, highlight mine, ")
          ))
        )
      }
    }
    
    ui
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
                 menuSubItem("Board", tabName = "profile_general"),
                 menuSubItem("Add New Video Analysis", tabName = "profile_video_analysis"),
                 menuSubItem("Load External Data", tabName = "load_external_data"))
      )
    } else { NULL }
  })
  
  # NEW VIDEO ANALYSIS (NVA) -----
  
  rv_nva <- reactiveValues(
    run_distance            = NULL, 
    run_pace_min_mile       = NULL,
    run_pace_min_km         = NULL,
    run_pace_km_hr          = NULL,
    run_pace                = NULL,
    summary_analysis_toggle = NULL,
    report_toggle           = TRUE, # NULL
    report_data_kpi         = NULL
  )
  
  # Go to new analysis tab when image is clicked
  observeEvent(input$new_video_analysis_button, {
    updateTabItems(session, "tab_profile", "profile_video_analysis")
    
    # Disable all tabs, only allow buttons to go to next
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tab_new_video_analysis li a[data-value=run_info]")
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tab_new_video_analysis li a[data-value=upload_video]")
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tab_new_video_analysis li a[data-value=select_frames]")
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tab_new_video_analysis li a[data-value=analyse_frames]")
    toggleClass(condition = TRUE,
                class = "disabled",
                selector = "#tab_new_video_analysis li a[data-value=report]")
  })
  
  # NVA: RUN INFO -----
  
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
    
    updateTabItems(session, "tab_new_video_analysis", "upload_video")
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
    updateTabItems(session, "tab_new_video_analysis", "select_frames")
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
          user_id         = rv_user$user_id,
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
        updateTabItems(session, "tab_new_video_analysis", "analyse_frames")
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
        actionButton("next_frame", "Save and Continue to Report", 
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
      reset("left_ground_pos") # Reset such that after air in front is default
      reset("left_ground_land") # Reset such that after air landing is default
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
      reset("right_ground_pos") # Reset such that after air in front is default
      reset("right_ground_land") # Reset such that after air landing is default
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
      rv_nva$summary_analysis_toggle <- TRUE
    }
  })
  
  observeEvent(rv_nva$summary_analysis_toggle, {
    if(!is.null(rv_nva$summary_analysis_toggle)) {
      
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
          air_ratio    = frames_air / num_frames,
          ground_ratio = frames_ground / num_frames,
          front_ratio  = frames_infront / frames_ground,
          behind_ratio = frames_behind / frames_ground,
          section_time = (num_frames / rv_vid$frame_rate_orig),
          step_rate    = 60 * steps / section_time,
          step_length  = (1000 * rv_nva$run_pace_km_hr / 60) / step_rate
        ) %>% 
        select(-num_frames)
      
      x <- 5
      
      rv_db$frame_upload[rv_db$frame_upload$frame_upload_id == rv_frame$frame_upload_id, 
                  c("distance", 
                    "pace_min_mile", "pace_min_km", "pace_km_hr", 
                    "frames_air", "frames_ground", 
                    "frames_infront", "frames_behind", 
                    "frames_pos_flat", "frames_pos_land", "frames_pos_takeoff", 
                    "frames_land_flat", "frames_land_heel", "frames_land_toe", 
                    "air_ratio", "ground_ratio", 
                    "front_ratio", "behind_ratio", 
                    "steps", "section_time", "step_rate", "step_length")] <- list(
                      rv_nva$run_distance, 
                      rv_nva$run_pace_min_mile, rv_nva$run_pace_min_km, rv_nva$run_pace_km_hr,
                      analysis$frames_air, analysis$frames_ground, 
                      analysis$frames_infront, analysis$frames_behind, 
                      analysis$frames_pos_flat, analysis$frames_pos_land, analysis$frames_pos_takeoff, 
                      analysis$frames_land_flat, analysis$frames_land_heel, analysis$frames_land_toe, 
                      analysis$air_ratio, analysis$ground_ratio, 
                      analysis$front_ratio, analysis$behind_ratio, 
                      analysis$steps, analysis$section_time, analysis$step_rate, analysis$step_length
                    )
      
      # Write the database
      source("R/write_data_reactive.R", local = TRUE)
      
      # Go to next tab
      updateTabItems(session, "tab_new_video_analysis", "report")
    }
  })
  
  # NVA: REPORT -----
  
  output$report_step_ui <- renderUI({
    if(!is.null(rv_nva$report_toggle)) {
      # Subset kpi data
      rv_nva$report_data_kpi <- rv_db$frame_upload[rv_db$frame_upload$frame_upload_id == rv_frame$frame_upload_id, ] 
      
      # UI for KPI values
      list(
        fluidRow(
            column(width = 6, 
                   valueBoxOutput("box_air_ratio"), 
                   tags$style("#box_air_ratio {width:98%;}")),
            column(width = 6, 
                   valueBoxOutput("box_ground_ratio"), 
                   tags$style("#box_ground_ratio {width:98%;}"))),
        fluidRow(
            column(width = 6, 
                   valueBoxOutput("box_behind_ratio"), 
                   tags$style("#box_behind_ratio {width:98%;}")),
            column(width = 6, 
                   valueBoxOutput("box_front_ratio"), 
                   tags$style("#box_front_ratio {width:98%;}"))),
        fluidRow(
            column(width = 6, 
                   valueBoxOutput("box_step_rate"), 
                   tags$style("#box_step_rate {width:98%;}")),
            column(width = 6, 
                   valueBoxOutput("box_step_length"), 
                   tags$style("#box_step_length {width:98%;}")))
      )
    } else { NULL }
  })
  
  output$box_air_ratio <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$air_ratio * 100), "%"), "Air time", 
        icon = icon("leaf"), color = "light-blue"
      )
    } else { NULL }
  })
  
  output$box_ground_ratio <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$ground_ratio * 100), "%"), "Ground time", 
        icon = icon("anchor"), color = "yellow"
      )
    } else { NULL }
  })
  
  output$box_front_ratio <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$front_ratio * 100), "%"), "Foot in front", 
        icon = icon("mail-forward"), color = "olive"
      )
    } else { NULL }
  })
  
  output$box_behind_ratio <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$behind_ratio * 100), "%"), "Foot behind", 
        icon = icon("mail-reply"), color = "red"
      )
    } else { NULL }
  })
  
  output$box_step_rate <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$step_rate)), "Step Rate (steps/minute)", 
        icon = icon("paw"), color = "green"
      )
    } else { NULL }
  })
  
  output$box_step_length <- renderValueBox({
    if(!is.null(rv_nva$report_toggle)) {
      valueBox(
        paste0(round(rv_nva$report_data_kpi$step_length, 2), "m"), "Step Length", 
        icon = icon("arrows-h"), color = "navy"
      )
    } else { NULL }
  })
  
  # REPORT PLOTS
  
  output$report_plot_1 <- renderPlot({
    if(!is.null(rv_nva$report_toggle)) {
      # Air time versus speed coloured by distance
      rv_db$frame_upload %>% 
        transmute(`Speed (km/hr)`                   = pace_km_hr,
                  `Air Ratio (percentage air time)` = 100 * air_ratio,
                  Distance                          = factor(distance)) %>% 
        na.omit %>% 
        ggplot(aes(x = `Speed (km/hr)`, y = `Air Ratio (percentage air time)`, col = Distance)) + 
          geom_point(alpha = 0.7) + 
          theme_bw() + 
          scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), minor_breaks = seq(5, 95, 10)) +
          scale_colour_manual(values = c("Your run" = "#e41a1c", run_distances_palette)) + 
          geom_point(data = rv_nva$report_data_kpi, 
                     aes(x = pace_km_hr, y = 100 * air_ratio, col = "Your run"),
                     size = 2, alpha = 0.9)
      
    } else { NULL }
  })
  
  output$report_plot_2 <- renderPlot({
    if(!is.null(rv_nva$report_toggle)) {
      # Step rate versus speed coloured by distance
      rv_db$frame_upload %>% 
        transmute(`Speed (km/hr)`                = pace_km_hr,
                  `Step Rate (steps per minute)` = step_rate,
                  Distance                       = factor(distance)) %>% 
        na.omit %>% 
        ggplot(aes(x = `Speed (km/hr)`, y = `Step Rate (steps per minute)`, col = Distance)) + 
          geom_point(alpha = 0.7) + 
          theme_bw() + 
          scale_colour_manual(values = c("Your run" = "#e41a1c", run_distances_palette)) + 
          geom_point(data = rv_nva$report_data_kpi, 
                     aes(x = pace_km_hr, y = step_rate, col = "Your run"),
                     size = 2, alpha = 0.9)
      
    } else { NULL }
  })
  
  output$report_plot_3 <- renderPlot({
    if(!is.null(rv_nva$report_toggle)) {
      # Behind ratio versus Pace (speed) coloured by distance
      rv_db$frame_upload %>% 
        transmute(`Speed (km/hr)`                                           = pace_km_hr,
                  `Percentage of time your ground foot is behind your body` = 100 * behind_ratio,
                  Distance                                                  = factor(distance)) %>% 
        na.omit %>% 
        ggplot(aes(x   = `Speed (km/hr)`, 
                   y   = `Percentage of time your ground foot is behind your body`, 
                   col = Distance)) + 
        geom_point(alpha = 0.7) + 
        theme_bw() + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), minor_breaks = seq(5, 95, 10)) +
        scale_colour_manual(values = c("Your run" = "#e41a1c", run_distances_palette)) + 
        geom_point(data = rv_nva$report_data_kpi, 
                   aes(x = pace_km_hr, y = 100 * behind_ratio, col = "Your run"),
                   size = 2, alpha = 0.9)
     
    } else { NULL }
  })
  
  output$report_plot_4 <- renderPlot({
    if(!is.null(rv_nva$report_toggle)) {
      # Behind ratio versus step rate coloured by distance
      rv_db$frame_upload%>% 
        transmute(`Step Rate (steps per minute)`                            = step_rate,
                  `Percentage of time your ground foot is behind your body` = 100 * behind_ratio,
                  Distance                                                  = factor(distance)) %>% 
        na.omit %>% 
        ggplot(aes(x   = `Step Rate (steps per minute)`, 
                   y   = `Percentage of time your ground foot is behind your body`, 
                   col = Distance)) +
        geom_point(alpha = 0.7) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), minor_breaks = seq(5, 95, 10)) +
        theme_bw() + 
        scale_colour_manual(values = c("Your run" = "#e41a1c", run_distances_palette)) + 
        geom_point(data = rv_nva$report_data_kpi, 
                   aes(x = step_rate, y = 100 * behind_ratio, col = "Your run"),
                   size = 2, alpha = 0.9)
      
    } else { NULL }
  })
  
  output$report_plot_5 <- renderPlot({
    if(!is.null(rv_nva$report_toggle)) {
      # Step length versus BMI (body weight / height^2) coloured by injury
      plot_data <- left_join(
        select(rv_db$frame_upload, frame_upload_id, user_id, `Step Rate` = step_rate),
        select(rv_db$user_info, user_id, BMI = runner_bmi, starts_with("injuries")),
        by = "user_id"
      ) 
      inj_names <- names(plot_data[-1:-4])
      plot_data[-1:-4] <- map2(plot_data[-1:-4], str_sub(inj_names, 10, -1),
                               function(col, name) {
                                 if_else(col, name, "")
                               })
      plot_data <- plot_data %>% 
        unite_(inj_names, col = "Injury", sep = " & ") %>% 
        mutate(Injury = factor(if_else(Injury == " & ", "None", Injury))) %>% 
        na.omit
      
      ggplot(plot_data, aes(x = `Step Rate`, y = BMI, col = Injury)) +
        geom_point(alpha = 0.7) + 
        # labs(y = expression(BMI  - (kg/m[2]))) +
        ylab(bquote('BMI (kg/m ' ^2~')')) + 
        theme_bw() + 
        scale_colour_brewer(palette = "Dark2") + 
        geom_point(data = plot_data[plot_data$frame_upload_id == 2, ], #rv_frame$frame_upload_id, ], 
                   aes(x = `Step Rate`, y = BMI, col = "Your run"),
                   size = 2, alpha = 0.9)
      
    } else { NULL }
  })
  
}) # End server function
