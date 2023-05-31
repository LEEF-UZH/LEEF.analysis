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

  flowcam_biomass_species <- c("Chlamydomonas","ChlamydomonasClumps","DividingChlamydomonas", # species for which biomass is calculated
                               "Cosmarium","Desmodesmus","DesmodesmusClumps",
                               "Monoraphidium", "Small_cells",
                               "Staurastrum1", "Staurastrum2")

  # Median Chlamy and Desmo based on classifier data

  ChlamyMedianWidth <- 5.8
  DesmoMedianWidth <- 7.5

  # Biomass calculation of normal cases

  algae_traits_normalCases <- algae_traits %>%
    dplyr::filter(species %in% c("Chlamydomonas",
                                 "Desmodesmus",
                                 "Small_cells")) %>%
    mutate(height = ifelse(species == "Desmodesmus", width/4,  width),
           biomass=(4/3)*pi*(width/2)*(height/2)*(length/2), # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density
  # Special cases

  ## 2 ellipsoids

  algae_traits_2ellipsoids <- algae_traits %>%
    dplyr::filter(species %in% c("DividingChlamydomonas",
                                 "Cosmarium",
                                 "Staurastrum2")) %>%
    mutate(height = length/4,
           biomass=2 * (4/3)*pi*(width/2)*height*(length/4), # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density

  ## 2 truncated cones

  algae_traits_2cones <- algae_traits %>%
    dplyr::filter(species %in% c("Staurastrum1")) %>%
    mutate(height = length/2,
           biomass=2 * (1/3)*pi*(width/2)*((length/6)^2 + (length/6)*(length/2) + (length/2)^2), # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density

  ## ChlamyClumps

  algae_traitsChlamyClumps <- algae_traits %>%
    dplyr::filter(species %in% c("ChlamydomonasClumps")) %>%
    mutate(height=ChlamyMedianWidth,
           biomass=area_abd*height, # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density

  ## DesmoClumps

  algae_traitsDesmoClumps <- algae_traits %>%
    dplyr::filter(species %in% c("DesmodesmusClumps")) %>%
    mutate(height=DesmoMedianWidth,
           biomass=area_abd*height, # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density


  ## Monoraphidium

  algae_traits_mono <- algae_traits %>%
    dplyr::filter(species %in% c("Monoraphidium")) %>%
    mutate(height = geodesic_thickness,
           biomass=(4/3)*pi*(geodesic_thickness/2)*(height/2)*(geodesic_length/2), # calculate biomass
           biomass=biomass/10^12)  # change it from um3 to g, assuming water density

  ## not biomass

  algae_traitsNotBiomass <- algae_traits %>%
    dplyr::filter(!(species %in% flowcam_biomass_species)) %>%
    mutate(height=as.numeric(NA),
           biomass=as.numeric(NA))


  # join datasets again

  algae_traits <- rbind(algae_traits_normalCases,
                        algae_traits_2ellipsoids,
                        algae_traits_2cones,
                        algae_traitsChlamyClumps,
                        algae_traitsDesmoClumps,
                        algae_traits_mono,
                        algae_traitsNotBiomass) # traits dataset finished here


  # calculate biomass per milliliter

  biomasses <- algae_traits %>%
    group_by(timestamp, bottle, species) %>%
    summarize(
      biomass = sum(biomass, na.rm = TRUE),
      volume_imaged = mean(volume_imaged)
    ) %>%
    mutate(
      biomass = biomass/volume_imaged,
      biomass = ifelse(!(species %in% flowcam_biomass_species), NA, biomass)
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
