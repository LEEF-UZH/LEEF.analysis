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
  as.Date(as.character(timestamp), "%Y%m%d")
}

fix_bottle <- function(bottle) {
  bottle[which(bottle == "b_100")] <- "b_c_1"
  bottle[which(bottle == "b_101")] <- "b_c_2"
  return(bottle)
}

#' Defines sets of species
#'
#' @param species_set_id if \code{NULL}, return the names of the species sets defined.
#'   If the name or the index of a specific set, return that set.
#'
#' @return Names of sets or the set itself
#' @export
#'
#' @examples
species_set <- function(species_set_id = NULL){
  sets <- list(
    flowcam_algae = c(
      "Chlamydomonas",
      "Cosmarium",
      "Cryptomonas",
      "Desmodesmus",
      "Monoraphidium",
      "Staurastrum1",
      "Staurastrum2"
    ),
    flowcam_ciliates = c(
      "Coleps_irchel",
      "Coleps_viridis",
      "Colpidium",
      "Dexiostoma",
      "Loxocephallus",
      "OtherCiliates",
      "Tetrahymena",
      "OtherCiliate"
    ),
    flowcam_rest = c(
      "airbubbles",
      "ChlamydomonasClumps",
      "ColpidiumVacuoles",
      "DividingChlamydomonas",
      "Debris",
      "DigestedAlgae",
      "Small_unidentified"
    ),
    LEEF_2_flowcam_algae = c(
      "Chlamydomonas",
      "Cryptomonas",
      "ChlamydomonasClumps",
      "DividingChlamydomonas",
      "Small_unidentified",
      "DigestedAlgae"
    ),
    LEEF_2_flowcam_rest = c(
      "Loxocephallus",
      "airbubbles",
      "ColpidiumVacuoles",
      "Debris",
      "OtherCiliate",
      "Coleps_irchel",
      "Colpidium"
    )
  )
  if (is.null(species_set_id)) {
    return(names(sets))
  } else {
    return(sets[[species_set_id]])
  }
}



