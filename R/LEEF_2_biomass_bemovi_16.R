#' LEEF-1 - Add biomass to traits
#'
#' @param ciliate_traits_16 traits as read from file \code{morph_mvt_TIMESTAMP.rds}
#' @param ciliate_density_16 density as read from file \code{mean_density_per_ml_TIMESTAMP.rds}
#'
#' @return list containing two objects, \code{traits} containing complete traits file
#'   as the argument \code{algai_traits} day includinc biomass column and \code{biomasses}
#'   per timestamp, bottle and species per milliliter.
#' @export
#'
#' @examples
LEEF_1_biomass_bemovi_16 <- function(
    ciliate_traits_16,
    ciliate_density_16
){

  video_biomass_species <- c(
    "Coleps_irchel", # species for which biomass is calculated
    "Colpidium",
    "Stylonychia2",
    "Paramecium_caudatum",
    "Paramecium_bursaria",
    "Euplotes",
    "Loxocephallus"
  )

  ciliate_traits_16_normalCases <- ciliate_traits_16 %>%
    dplyr::filter(species %in% video_biomass_species) %>%
    mutate(
      height = ifelse(
        species %in% c("Euplotes", "Stylonychia2"),
        mean_minor / 3,
        ifelse(
          species %in% c("Paramecium_bursaria"),
          mean_minor / 1.5,
          mean_minor
        )
      ),
      biomass = (4 / 3) * pi * (mean_minor / 2) * (height / 2) * (mean_major / 2), # calculate biomass
      biomass = biomass / 10^12 # change it from um3 to g, assuming water density
    )
  

  ciliate_traits_16_NotBiomass <- ciliate_traits_16 %>%
    dplyr::filter(!(species %in% video_biomass_species)) %>%
    mutate(
      height=as.numeric(NA),
      biomass=as.numeric(NA)
    )

  # join datasets again

  ciliate_traits_16 <- rbind(
    ciliate_traits_16_normalCases,
    ciliate_traits_16_NotBiomass
    ) # traits dataset finished here


  # calculate biomass per milliliter

  extrapolation.factor_16 <- 10.044

  # Make sure, that we have n_frames and not N_frames
  names(ciliate_traits_16)[names(ciliate_traits_16) == "N_frames"] <- "n_frames"
  
  biomasses <- ciliate_traits_16 %>%
    group_by(timestamp, bottle, species) %>%
    summarize(biomass = sum(biomass * n_frames, na.rm = TRUE) / (3*125)) %>%
    mutate(
      biomass = biomass*extrapolation.factor_16,
      biomass = ifelse(
        !(species %in% video_biomass_species),
        as.numeric(NA),
        biomass
      )
    ) # add biomass=NA if not a species


  biomasses$timestamp <- as.character(biomasses$timestamp)

  densities <- full_join(
    ciliate_density_16,
    biomasses,
    by = c("timestamp", "bottle", "species")
  )
  densities$biomass[densities$species %in% video_biomass_species & is.na(densities$biomass)] <- 0
    # mutate(
    #   biomass = case_when(
    #     species %in% flowcam_biomass_species & is.na(biomass) ~ 0,
    #     TRUE ~ biomass)
    # )

  return(
    list(
      traits = ciliate_traits_16,
      density = densities
    )
  )

}
