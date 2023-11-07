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
CalculateDensities <- function(morph, meas, extrapolation.factor, cropping.factor) {
    density <- morph %>%
        group_by(timestamp, bottle, species, file) %>%
        summarize(count = sum(n_frames)) %>%
        # summarize(count=sum(N_frames)) %>%
        mutate(dens.ml = count * extrapolation.factor * cropping.factor) %>%
        na.omit() %>%
        group_by(timestamp, bottle, species) %>%
        summarise(
            density = sum(dens.ml) / (length(unique(file)) * 125)
        ) %>%
    mutate(measurement=meas)
    return(density)
}
