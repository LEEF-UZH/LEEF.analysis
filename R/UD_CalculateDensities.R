#' Re-calculate densities
#'
#' @param morph
#' @param meas
#'
#' @return
#'
#' @importFrom dplyr group_by summarize mutate
#' @import data.table
#' @importFrom stats na.omit
#' @export
#'
#' @examples
CalculateDensities <- function(morph, meas) {
  density <- morph %>%
    group_by(timestamp, bottle, species, file, date, temperature_treatment, magnification, sample) %>%
    summarize(count = sum(n_frames)) %>%
    # summarize(count=sum(N_frames)) %>%
    mutate(dens.ml = count * extrapolation.factor * cropping.factor) %>%
    na.omit() %>%
    group_by(timestamp, date, species, bottle, temperature_treatment, magnification, sample) %>%
    summarise(
      numberOfVideos = length(unique(file)),
      density = sum(dens.ml) / (numberOfVideos * 125)
    )
  return(density)
}
