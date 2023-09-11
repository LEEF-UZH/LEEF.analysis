#' LEEF-2 - Add biomass to traits
#'
#' @param algae_traits algae traits as read from file \code{algae_traits_TIMESTAMP.rds}
#' @param algae_density algae density as read from file \code{algae_density_TIMESTAMP.rds}
#'
#' @return list containing two objects, \code{traits} containing complete traits file
#'   as the argument \code{algai_traits} day includinc biomass column and \code{biomasses}
#'   per timestamp, bottle and species per milliliter.
#' @export
#'
#' @examples
LEEF_2_biomass_flowcam <- function(
    algae_traits,
    algae_density
){

  algae_traits <- algae_traits %>%
    dplyr::filter(!(species %in% c("Chlamydomonas","Small_cells") & area_abd >500)) %>% # filter out individuals that are too big for these two classes
    collect()

  # species for which biomass is calculated
  flowcam_biomass_species <- c(
    "Chlamydomonas", "ChlamydomonasClumpsSmall", "ChlamydomonasClumpsLarge", "DividingChlamydomonas", 
    "Small_cells"
  )

  # Median Chlamy and Desmo based on classifier data

  ChlamyMedianWidth <- 5.8
  DesmoMedianWidth <- 7.5

  # Biomass calculation of normal cases
  algae_traits_normalCases <- algae_traits %>%
    dplyr::filter(species %in% c(
      "Chlamydomonas",
      "Small_cells"
    )) %>%
    mutate(
      height = width,
      biomass = (4 / 3) * pi * (width / 2) * (height / 2) * (length / 2), # calculate biomass
      biomass = biomass / 10^12
    ) # change it from um3 to g, assuming water density

# special cases

## clumps
  algae_traits_clumps <- algae_traits %>%
    dplyr::filter(
      species %in% c(
        "ChlamydomonasClumpsSmall",
        "ChlamydomonasClumpsLarge"
      )
    ) %>%
    mutate(
      height = ChlamyMedianWidth,
      biomass = area_abd*height, # calculate biomass
      biomass = biomass/10^12  # change it from um3 to g, assuming water density
    ) 

## double ellipsoids

  algae_traits_2ellipsoids <- algae_traits %>%
    dplyr::filter(species %in% c(
      "DividingChlamydomonas"
    )) %>%
    mutate(
      height = length / 4,
      biomass = 2 * (4 / 3) * pi * (width / 2) * height * (length / 4), # calculate biomass
      biomass = biomass / 10^12 # change it from um3 to g, assuming water density
    )


  ## not biomass

  algae_traits_NotBiomass <- algae_traits %>%
    dplyr::filter(!(species %in% flowcam_biomass_species)) %>%
    mutate(
      height = as.numeric(NA),
      biomass = as.numeric(NA)
    )


  # join datasets again

  algae_traits <- rbind(
    algae_traits_normalCases,
    algae_traits_clumps,
    algae_traits_2ellipsoids,
    algae_traits_NotBiomass
  ) # traits dataset finished here


  # calculate biomass per milliliter

  biomasses <- algae_traits %>%
    group_by(timestamp, bottle, species) %>%
    summarize(
      biomass = sum(biomass, na.rm = TRUE),
      volume_imaged = mean(volume_imaged)
    ) %>%
    mutate(
      biomass = biomass/volume_imaged,
      biomass = ifelse(!(species %in% flowcam_biomass_species), as.numeric(NA), biomass)
    ) %>% # add biomass=NA if not a species
    select(-volume_imaged)

  densities <- full_join(
      algae_density,
      biomasses,
      by = c("timestamp", "bottle", "species")
    )
  densities$biomass[densities$species %in% flowcam_biomass_species & is.na(densities$biomass)] <- 0
    # mutate(
    #   biomass = case_when(
    #     species %in% flowcam_biomass_species & is.na(biomass) ~ 0,
    #     TRUE ~ biomass)
    # )

  return(
    list(
      traits = algae_traits,
      density = densities
    )
  )

}
