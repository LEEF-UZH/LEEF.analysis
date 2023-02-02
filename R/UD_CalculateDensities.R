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
CalculateDensities <- function(
    morph,
    meas
    ){
  density <- morph %>%
    dplyr::group_by(timestamp, bottle, species, file) %>%
    dplyr::summarize(count=sum(n_frames)) %>%
    # summarize(count=sum(N_frames)) %>%
    dplyr::mutate(dens.ml = count * extrapolation.factor * cropping.factor) %>%
    stats::na.omit() %>%
    dplyr::group_by(timestamp, bottle, species) %>%
    dplyr::summarize(density = sum(dens.ml)/(3*125)) %>%
    dplyr::mutate(measurement=meas)
  return(density)
}
