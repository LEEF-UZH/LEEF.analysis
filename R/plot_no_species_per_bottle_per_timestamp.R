#' Plot diagnostic plot to check number of species per timestamp
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
plot_no_species_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  config_yml = system.file("tables.yml", package = "LEEF.analysis"),
  config
){
  if (missing(config)) {
    config <- yaml::read_yaml(config_yml)
  }

  data <- read_sql(db, sql = config$species_per_bottle$SQL)
  data$timestamp <- as.Date(data$timestamp, "%Y%m%d")
  data$bottle[which(data$bottle == "b_100")] <- "b_c_1"
  data$bottle[which(data$bottle == "b_101")] <- "b_c_2"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$no_species)) +
    ggplot2::geom_point() +
    ggplot2::xlab("") +
    ggplot2::facet_wrap(~measurement)
  p
}
