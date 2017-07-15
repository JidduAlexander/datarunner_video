
# If the file has changed then read it and also change the mtime

# User
if(rv_db$user_mtime != file.mtime("input/db/db_user.Rds")) {
  rv_db$user        <- readRDS("input/db/db_user.Rds")
  rv_db$user_mtime  <- file.mtime("input/db/db_user.Rds")
}

# User info
if(rv_db$user_info_mtime != file.mtime("input/db/db_user_info.Rds")) {
  rv_db$user_info        <- readRDS("input/db/db_user_info.Rds")
  rv_db$user_info_mtime  <- file.mtime("input/db/db_user_info.Rds")
}

# Injuries
if(rv_db$injury_mtime != file.mtime("input/db/db_injury.Rds")) {
  rv_db$injury        <- readRDS("input/db/db_injury.Rds")
  rv_db$injury_mtime  <- file.mtime("input/db/db_injury.Rds")
}

# Frame
if(rv_db$frame_mtime != file.mtime("input/db/db_frame.Rds")) {
  rv_db$frame        <- readRDS("input/db/db_frame.Rds")
  rv_db$frame_mtime  <- file.mtime("input/db/db_frame.Rds")
}

# Frame Upload
if(rv_db$frame_upload_mtime != file.mtime("input/db/db_frame_upload.Rds")) {
  rv_db$frame_upload        <- readRDS("input/db/db_frame_upload.Rds")
  rv_db$frame_upload_mtime  <- file.mtime("input/db/db_frame_upload.Rds")
}
