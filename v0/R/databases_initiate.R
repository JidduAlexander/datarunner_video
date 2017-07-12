library(tibble)
library(purrr)
library(sodium)

# User database

db_user <- tibble(
  user_id     = c(1),
  name        = c("Jiddu"),
  email       = c("san@nesware.net"),
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
    runner_sex                     = "Male",
    injuries_ankle                 = FALSE,
    injuries_knee                  = FALSE,
    deal_with_injuries_pain_run    = FALSE,
    deal_with_injuries_stop_run    = FALSE,
    deal_with_injuries_long_break  = FALSE,
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
    input_form_correct             = TRUE
)

# Frame database

db_frame <- tibble(
  frame_id                 = 1,
  frame_upload_id          = 1,
  frame_num                = NA_real_,
  path                     = NA_character_,
  left_air_ground          = NA_character_,
  left_ground_pos          = NA_character_,
  left_ground_land         = NA_character_,
  left_ground_land_detail  = NA_character_,
  right_air_ground         = NA_character_,
  right_ground_pos         = NA_character_,
  right_ground_land        = NA_character_,
  right_ground_land_detail = NA_character_
)

# Frame upload database

db_frame_upload <- tibble(
  frame_upload_id = 1,
  user            = 1,
  date_upload     = date()
)


# Write databases
saveRDS(db_user, "input/db/db_user.Rds")
saveRDS(db_user_info, "input/db/db_user_info.Rds")
saveRDS(db_frame, "input/db/db_frame.Rds")
saveRDS(db_frame_upload, "input/db/db_frame_upload.Rds")

