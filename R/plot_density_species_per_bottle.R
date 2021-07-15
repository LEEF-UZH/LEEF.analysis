#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param measurement the measurement which should be used
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
plot_density_species_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  measurement = "bemovi_mag_16",
  config_yml = system.file("tables.yml", package = "LEEF.analysis"),
  config
){
  if (missing(config)) {
    config <- yaml::read_yaml(config_yml)
  }

  SQL <- config[[paste0(measurement, "_dens_species")]]$SQL
  data <- read_sql(db, sql = SQL)

  data$timestamp <- convert_timestamp(data$timestamp)
  data$bottle <- fix_bottle(data$bottle)

  ticks <- unique(data$timestamp)

  data$measurement <- sort_measurements(data$measurement)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$density)) +
    ggplot2::geom_line(aes(y = .data$density, colour = .data$species)) +
    scale_x_date(breaks = ticks) +
    ggplot2::facet_wrap(~bottle, ncol = 3) +
    ggplot2::scale_colour_manual(values = 1:20 ) +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  p
}
