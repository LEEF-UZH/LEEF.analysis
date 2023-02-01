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
#' @importFrom rlang !!
#' @import ggplot2
#'
#' @export
#'
#' @examples
LEEF_2_plot_conductivity_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){

  data <- db_read_conductivity(db) %>%
    dplyr::collect() %>%
    dplyr::mutate(conductivity = as.numeric(conductivity)) %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
    dplyr::mutate(bottle = fix_bottle(bottle))

  if (nrow(data) > 0) {
    data$label <- paste0("t: ", data$temperature, "\n r: ", data$resources, "\n s: ", data$salinity)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$day, y = .data$conductivity)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$conductivity)) +
      ggplot2::facet_grid(rows = vars(replicate), cols = vars(label), scales = "free_y") +
      ggplot2::geom_text(
        data = data,
        ggplot2::aes(x = -Inf, y = Inf, label = bottle, group = bottle),
        hjust = -0.5,
        vjust = 1.4,
        size = 3
      ) +
      ggplot2::scale_colour_manual(values = 1:40) +
      ggplot2::xlab("Day of Experiment") +
      ggplot2::ylab(expression("Conductivity (S/m)")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
    p
  } else {
    warning("No data for available!")
  }
}
