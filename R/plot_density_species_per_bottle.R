#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param measurement the measurement to be plotted. If \code{NULL},
#'   the default, they are plotted by temperature treatment (constant & increasing)
#' @param transform_density_4throot if \code{TRUE}, density is transformed using 4th root transformation.
#'
#' @return \code{ggplot} object of the plot
#'
#' @importFrom dplyr collect mutate
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot_density_species_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  transform_density_4throot = TRUE
){
  data <- db_read_density(db) %>%
    dplyr::collect() %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
    dplyr::mutate(exp_day = exp_day(timestamp)) %>%
    dplyr::mutate(bottle = fix_bottle(bottle)) %>%
    dplyr::mutate(
      density = if (transform_density_4throot) {
        exp(log(density)/4)
      } else {
        density
      }
    )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$exp_day, y = .data$density)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$density, colour = .data$species)) +
    ggplot2::facet_grid(rows = vars(measurement), cols = vars(temperature), scales = "free_y") +
    ggplot2::scale_colour_manual(values = 1:40) +
    ggplot2::xlab("Day of Experiment") +
    ggplot2::ylab(ifelse(transform_density_4throot, "4th root density", "density")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  p
}