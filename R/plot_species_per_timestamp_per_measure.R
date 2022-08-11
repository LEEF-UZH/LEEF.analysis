#' Plot diagnostic plot to check species per timestamp per method
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @importFrom dplyr collect mutate
#' @return \code{ggplot} object of the plot
#'
#' @import ggplot2
#'
#' @export
#'
plot_species_per_timestamp_per_measure <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){
  data <- db_read_density(db) %>%
    dplyr::collect() %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp))

  data$timestamp  <- data$timestamp + (as.integer(as.factor(data$measurement)) / 5) - 0.7

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$species)) +
    ggplot2::geom_point(aes(colour = .data$measurement)) +
    ggplot2::xlab("") +
    ggplot2::scale_colour_manual(values = 1:10 )
  p
}
