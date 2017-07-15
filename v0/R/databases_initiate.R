library(tibble)
library(purrr)
library(sodium)

# User database

db_user <- tibble(
  user_id     = 1,
  name        = "Jiddu",
  email       = "san@nesware.net",
  pw_hash     = map_chr(c("runner"), function(x) paste(as.character(hash(charToRaw(x))), collapse = "")),
  date_joined = date()
)

# User info database

db_user_info <- tibble(
    user_id                        = 1,
    name                           = "Jiddu",
    runner_yob                     = 1986,
    runner_height                  = 190,
    runner_weight                  = 70,
    runner_bmi                     = runner_weight / (runner_height / 100)^2,
    runner_sex                     = "Male",
    # injuries_ankle                 = FALSE,
    # injuries_knee                  = FALSE,
    # deal_with_injuries_pain_run    = FALSE,
    # deal_with_injuries_stop_run    = FALSE,
    # deal_with_injuries_long_break  = FALSE,
    aim_joy                        = TRUE,
    aim_health                     = TRUE,
    aim_amature                    = FALSE,
    aim_pro                        = FALSE,
    runner_experience              = 3,
    runner_runs_per_week           = 2,
    runner_average_weekly_distance = 20,
    training_type_home_jog         = TRUE,
    training_type_interval_sprint  = TRUE,
    training_type_treadmill        = FALSE,
    training_gym                   = 0,
    training_crossfit              = 2,
    training_stretch_before        = 0,
    training_stretch_after         = 2,
    input_form_correct             = TRUE
)

# User Injuries
db_injury <- tibble(
  user_id  = double(0),
  name     = character(0),
  run_type = character(0),
  affect   = character(0)
)

# Frame database

db_frame <- tibble(
  frame_id                 = numeric(0),
  frame_upload_id          = numeric(0),
  frame_num                = numeric(0),
  path                     = character(0),
  left_air_ground          = character(0),
  left_ground_pos          = character(0),
  left_ground_land         = character(0),
  left_ground_land_detail  = character(0),
  right_air_ground         = character(0),
  right_ground_pos         = character(0),
  right_ground_land        = character(0),
  right_ground_land_detail = character(0)
)

# db_frame2 <- tibble(
#   frame_id                 = 1,
#   frame_upload_id          = 1,
#   frame_num                = NA_real_,
#   path                     = NA_character_,
#   left_air_ground          = NA_character_,
#   left_ground_pos          = NA_character_,
#   left_ground_land         = NA_character_,
#   left_ground_land_detail  = NA_character_,
#   right_air_ground         = NA_character_,
#   right_ground_pos         = NA_character_,
#   right_ground_land        = NA_character_,
#   right_ground_land_detail = NA_character_
# )

# Frame upload database

db_frame_upload <- tibble(
  frame_upload_id    = numeric(0),
  user_id            = numeric(0),
  date_upload        = character(0),
  distance           = character(0),
  pace_min_mile      = numeric(0),
  pace_min_km        = numeric(0),
  pace_km_hr         = numeric(0),
  frames_air         = numeric(0), 
  frames_ground      = numeric(0), 
  frames_infront     = numeric(0),
  frames_behind      = numeric(0),
  frames_pos_flat    = numeric(0),
  frames_pos_land    = numeric(0),
  frames_pos_takeoff = numeric(0),
  frames_land_flat   = numeric(0),
  frames_land_heel   = numeric(0),
  frames_land_toe    = numeric(0),
  air_ratio          = numeric(0),
  ground_ratio       = numeric(0),
  front_ratio        = numeric(0),
  behind_ratio       = numeric(0),
  steps              = numeric(0),
  section_time       = numeric(0),
  step_rate          = numeric(0),
  step_length        = numeric(0)
)

# db_frame_upload2 <- tibble(
#   frame_upload_id    = 1,
#   user_id            = 1,
#   date_upload        = date(),
#   distance           = NA_character_,
#   pace_min_mile      = NA_real_,
#   pace_min_km        = NA_real_,
#   pace_km_hr         = NA_real_,
#   frames_air         = NA_real_,
#   frames_ground      = NA_real_,
#   frames_infront     = NA_real_,
#   frames_behind      = NA_real_,
#   frames_pos_flat    = NA_real_,
#   frames_pos_land    = NA_real_,
#   frames_pos_takeoff = NA_real_,
#   frames_land_flat   = NA_real_,
#   frames_land_heel   = NA_real_,
#   frames_land_toe    = NA_real_,
#   air_ratio          = NA_real_,
#   ground_ratio       = NA_real_,
#   front_ratio        = NA_real_,
#   behind_ratio       = NA_real_,
#   steps              = NA_real_,
#   section_time       = NA_real_,
#   step_rate          = NA_real_,
#   step_length        = NA_real_
# )


# Write databases
saveRDS(db_user, "input/db/db_user.Rds")
saveRDS(db_user_info, "input/db/db_user_info.Rds")
saveRDS(db_injury, "input/db/db_injury.Rds")
saveRDS(db_frame, "input/db/db_frame.Rds")
saveRDS(db_frame_upload, "input/db/db_frame_upload.Rds")

