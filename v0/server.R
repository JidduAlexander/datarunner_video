

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # DATABASE -----
  
  # Initialise databases
  rv_db <- reactiveValues(
    user         = db_user,
    frame        = db_frame,
    frame_upload = db_frame_upload,
    # Modification times to check reloading
    user_mtime          = db_user_mtime,
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
      paste("Hello,", rv_user$user_name)
    } else { NULL }
  })
  
  # UI for login in or out
  output$login_ui <- renderUI({
    if(is.null(rv_user$user_id)) {
      list(
        fluidRow( 
          column(width = 3, h6("User name")),
          column(width = 3, h6("Password")),
          column(width = 6)
        ),
        fluidRow(
          column(width = 3, textInput("user_name", NULL)),
          column(width = 3, textInput("user_pw", NULL)),
          column(width = 2, withBusyIndicatorUI(actionButton("user_login", "Login",
                                                             class = "btn-primary"))),
          column(width = 1),
          column(width = 3, withBusyIndicatorUI(actionButton("create_account", "Create Account",
                                                             class = "btn-primary")))
        )
      )
    } else {
      list(
        p(""),
        fluidRow(
          column(width = 3),
          column(width = 5, div(style = "float:right;", textOutput("welcome_msg"))),
          column(width = 1),
          column(width = 3, withBusyIndicatorUI(actionButton("user_logout", "Logout",
                                                             class = "btn-primary")))
        )
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
          
          Sys.sleep(0.5)
          
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
  
  # Reactive video variables
  rv_vid <- reactiveValues(
    inFile      = NULL,
    temp_file   = NULL,
    frame_rate  = NULL,
    frames      = NULL,
    frame_start = NULL,
    frame_end   = NULL,
    loaded      = FALSE
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
      frm_info <- system(paste('ffprobe -v error -select_streams v:0 -show_entries stream=avg_frame_rate,nb_frames',
                               ' -of default=noprint_wrappers=1:nokey=1', 
                               rv_vid$inFile$datapath), 
                         intern = TRUE)
      fr <- frm_info[1] %>% 
        str_split(pattern = "/") %>% 
        unlist() %>% 
        as.numeric()
      rv_vid$frame_rate <- fr[1]/fr[2]
      rv_vid$frames      <- as.numeric(frm_info[2])
      rv_vid$frame_start <- floor(input$upload_start_time * rv_vid$frame_rate)
      rv_vid$frame_end   <- rv_vid$frame_start + rv_vid$frames - 1
      
      # Copy video and paste with frame numbers
      system(paste('ffmpeg -r 30 -i', rv_vid$inFile$datapath, 
                   '-ss ', input$upload_start_time,
                   '-t', input$upload_end_time - input$upload_start_time,
                   # Set font and text location
                   '-vf  "drawtext=fontfile=/Windows/Fonts/arial.ttf: x=(w-tw)/20: y=20: ',
                   paste('fontsize=', fontsize, ':'), # set fontsize
                   # Text n means write frame number (n) 
                   'fontcolor=white: box=1: boxcolor=0x00000000@1: text=%{n}:"',
                   '-an -y',
                   paste0("www/temp/", rv_vid$temp_file))) # Output file name
    })
    
    rv_vid$loaded <- TRUE
  })
  
  # Render video
  output$upload_show_video <- renderUI({
    rv_vid$inFile
    if(isolate(rv_vid$loaded)) {
      list(
        fluidRow(
          column(width = 3),
          # Forward and backward frame buttons
          column(width = 2,
                 div(style='float:left;', actionButton("back0", NULL, icon = icon("chevron-left"))), 
                 actionButton("forward0", NULL, icon = icon("chevron-right")),
                 p("Move 1 frame (not in Chrome)")),
          column(width = 7, 
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
                    "),
        # Video output in HTML
        HTML(paste('<center><video id="v0" controls tabindex="0" height = "600px">',
                   '<source type="video/webm; codecs=&quot;vp8, vorbis&quot;" ',
                   paste0('src="temp/', rv_vid$temp_file, '">'),
                   '</source></center>'))
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
  
  # FRAME SELECTION -----
  
  rv_frame <- reactiveValues(
    selection         = NULL,
    frame_upload_id   = NULL,
    current_frame_num = NULL
  )
  
  output$video_frame_start_ui <- renderUI({
    if(!is.null(rv_vid$frame_end)) {
      numericInput("video_frame_start", NULL, value = 0, min = 0, max = rv_vid$frame_end)
    } else { NULL }
  })
  
  output$video_frame_end_ui <- renderUI({
    if(!is.null(rv_vid$frame_end)) {
      numericInput("video_frame_end", NULL, value = 1, min = 0, max = rv_vid$frame_end)
    } else { NULL }
  })
  
  # Frames to images
  observeEvent(input$video_frame_button, {
    withBusyIndicatorServer("video_frame_button", {
      withProgress(message = 'Preparing frame images', {
        
        rv_frame$selection <- input$video_frame_start:input$video_frame_end
        
        # Video Loading parameters
        start <- (input$video_frame_start - 0.5) / rv_vid$frame_rate
        end <- input$video_frame_end / rv_vid$frame_rate
        
        start_hr   <- as.character(start %/% 3600)
        start_min  <- as.character(start %% 3600 %/% 60)
        start_sec  <- as.character(start %% 3600 %% 60)
        
        end_hr   <- as.character(end %/% 3600)
        end_min  <- as.character(end %% 3600 %/% 60)
        end_sec  <- as.character(end %% 3600 %% 60)
        
        duration <- end - start
        
        seq_frames <- 1:(input$video_frame_end + 1 - input$video_frame_start)
        
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
      })
    })
  })
  
  # FRAME ANALYSIS -----
  
  output$frame_analysis_img <- renderUI({
    if(!is.null(rv_frame$selection)) {
      # Read the database
      source("R/read_data_reactive.R", local = TRUE)
      
      path <- isolate(rv_db$frame) %>% 
        filter(frame_num == rv_frame$current_frame_num & frame_upload_id == rv_frame$frame_upload_id) %>% 
        select(path)
      
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
    
    # Reset radiobuttons
    reset("left_air_ground")
    reset("left_ground_pos")
    reset("left_ground_land")
    reset("left_ground_land_detail")
    reset("right_air_ground")
    reset("right_ground_pos")
    reset("right_ground_land")
    reset("right_ground_land_detail")

    if(rv_frame$current_frame_num != max(rv_frame$selection)) {
      # Current frame +1
      rv_frame$current_frame_num <- rv_frame$current_frame_num + 1
    } else {
      # Maybe set all reactive value to NULL?
      rv_frame$current_frame_num <- rv_frame$selection[1]
    }
  })
  
  
})
