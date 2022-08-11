#' Return names of all configs in config file
#'
#' @param config_yml the config file containing the sql queries
#'
#' @return \code{ggplot} object of the plot
#'
#' @import ggplot2
#'
#' @export
#'
configs <- function(
  config_yml = system.file("tables.yml", package = "LEEF.analysis")
){
  config <- yaml::read_yaml(config_yml)

  names(config)
}
