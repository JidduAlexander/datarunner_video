

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Reactive video variables
  rv_vid <- reactiveValues(
    inFile     = NULL,
    frame_rate = NULL,
    loaded     = FALSE
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
      frm_info <- system(paste('ffprobe -v error -select_streams v:0 -show_entries stream=avg_frame_rate',
                               ' -of default=noprint_wrappers=1:nokey=1', 
                               rv_vid$inFile$datapath), 
                         intern = TRUE)
      fr <- frm_info[1] %>% 
        str_split(pattern = "/") %>% 
        unlist() %>% 
        as.numeric()
      rv_vid$frame_rate <- fr[1]/fr[2]
      
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
                             min = 0.1, max = 10, value = 1, step = 0.1, ticks = FALSE,
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
  
})
