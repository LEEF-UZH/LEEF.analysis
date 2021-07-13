#' Plot diagnostic plot to check species per timestamp per method
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param config_yml the config file containing an sql query named \code{species_per_bottle}
#' @param config the configuration in \code{list} format containing an sql query named \code{species_per_bottle}.
#'   If not specified, the \code{config_yml} will be used
#'
#' @return \code{ggplot} object of the plot
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot_species_per_timestamp_per_measure <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  config_yml = system.file("tables.yml", package = "LEEF.analysis"),
  config
){
  if (missing(config)) {
    config <- yaml::read_yaml(config_yml)
  }

  data <- read_sql(db, sql = config$species_per_timestamp_measure$SQL)

  data$timestamp <- convert_timestamp(data$timestamp)
  # data$bottle <- fix_bottle(data$bottle)
  data$measurement <- sort_measurements(data$measurement)

  ticks <- unique(data$timestamp)

  data$timestamp  <- data$timestamp + (as.integer(data$measurement) / 5) - 1.1

  data$species[data$measurement == "flowcytometer"] <- paste0("_", data$species[data$measurement == "flowcytometer"])

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$species)) +
    ggplot2::geom_point(aes(colour = .data$measurement)) +
    ggplot2::xlab("") +
    ggplot2::scale_colour_manual(values = 1:10 ) +
    scale_x_date(breaks = ticks)
  p
}
