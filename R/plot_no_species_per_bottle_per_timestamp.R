#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return \code{ggplot} object of the plot
#'
#' @importFrom dplyr group_by summarise collect mutate
#' @import ggplot2
#'
#' @export
#'
plot_no_species_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){
  data <- db_read_density(db) %>%
    dplyr::group_by(timestamp, species, measurement) %>%
    dplyr::summarise() %>%
    dplyr::group_by(timestamp, measurement) %>%
    dplyr::summarise(no_species = dplyr::n()) %>%
    dplyr::collect() %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp))

  data$no_species  <- data$no_species + (as.integer(as.factor(data$measurement)) / 10)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$no_species)) +
    ggplot2::geom_line(aes(colour = .data$measurement)) +
    ggplot2::xlab("") +
    ggplot2::scale_colour_manual(values = 1:10 )
  p

  }
