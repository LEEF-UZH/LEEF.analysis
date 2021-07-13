sort_measurements <- function(measurement) {
  factor(
    measurement,
    levels=c(
      "bemovi_mag_16_morph", "bemovi_mag_16",
      "bemovi_mag_25_morph", "bemovi_mag_25",
      "bemovi_mag_25_cropped_morph", "bemovi_mag_25_cropped",
      "flowcam_traits", "flowcam",
      "flowcytometer",
      "manualcount",
      "o2meter"
    )
  )
}

convert_timestamp <- function(timestamp) {
  as.Date(timestamp, "%Y%m%d")
}

fix_bottle <- function(bottle) {
  bottle[which(bottle == "b_100")] <- "b_c_1"
  bottle[which(bottle == "b_101")] <- "b_c_2"
  return(bottle)
}
