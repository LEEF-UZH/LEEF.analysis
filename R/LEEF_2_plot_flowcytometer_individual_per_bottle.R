#' Plot diagnostic plot to check is plte has an impact on flowcytometer for LEEF-2
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param species species to be plotted
#' @return \code{ggplot} object of the plot
#'
#' @importFrom dplyr collect mutate filter select
#' @importFrom rlang !!
#' @import ggplot2
#'
#' @export
#'
#' @examples
LEEF_2_plot_flowcytometer_individual_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  species = c("bacteria")
){
  options(dplyr.summarise.inform = FALSE)

  data <- db_read_table(db, table = "flowcytometer__flowcytometer_density") %>%
    dplyr::filter(species %in% !!species) %>%
    dplyr::select(timestamp, plate, bottle, species, density) %>%
    dplyr::collect()
  if (nrow(data) < 1) {
    warning("No data for available!")
  } else {
    data <- data %>% dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
      dplyr::mutate(bottle = fix_bottle(bottle)) # %>%

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$density)) +
      ggplot2::geom_point(ggplot2::aes(y = .data$density, colour = .data$plate, shape = .data$plate), size = 2) +
      ggplot2::geom_line(ggplot2::aes(y = .data$density, colour = .data$plate)) +
      ggplot2::facet_wrap(~bottle, ncol = 3)  +
      ggplot2::scale_colour_manual(values = 1:40) +
      ggplot2::xlab("Timestamp of Experiment") +
      ggplot2::ylab(paste("density", species)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) +
      ggplot2::labs(plot.title = "Flowcytometer by timestamp and plate")

    p
  }
}
