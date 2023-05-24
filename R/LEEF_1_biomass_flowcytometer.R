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
LEEF_1_biomass_flowcytometer <- function(
    traits,
    density
){

  # calculate biomass per milliliter


  biomasses <- traits %>%
    group_by(timestamp, sample, bottle, volume, dilution_factor) %>%
    summarize(biomass = sum(biomass, na.rm = TRUE)) %>%
    mutate(biomass = biomass * 1000000 / volume * dilution_factor) %>%
    mutate(volume = NULL) %>%
    mutate(dilution_factor = NULL)

  densities <- full_join(
    density,
    biomasses,
    by = c("timestamp", "sample", "bottle")
  )

  return(
    list(
      traits = ciliate_traits_16,
      density = densities
    )
  )

}
