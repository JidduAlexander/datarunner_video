library(tibble)
library(purrr)
library(sodium)

# User database

db_user <- tibble(
  user_id     = c(1, 2),
  name        = c("Jiddu", "Boris"),
  email       = c("san@nesware.net", "bjlefeber@gmail.com"),
  pw_hash     = map_chr(c("runner", "blauwekomeet"), function(x) paste(as.character(hash(charToRaw(x))), collapse = "")),
  date_joined = c(date(), date())
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
saveRDS(db_frame, "input/db/db_frame.Rds")
saveRDS(db_frame_upload, "input/db/db_frame_upload.Rds")

