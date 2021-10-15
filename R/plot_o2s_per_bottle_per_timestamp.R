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
plot_o2s_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){

  data <- db_read_o2(db) %>%
    dplyr::collect() %>%
    dplyr::mutate(percent_o2 = replace(percent_o2, percent_o2 == "---", NA)) %>%
    dplyr::mutate(percent_o2 = as.numeric(percent_o2)) %>%
    dplyr::mutate(sensor = as.character(sensor)) %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
    dplyr::mutate(bottle = fix_bottle(bottle))


  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$day, y = .data$percent_o2)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$percent_o2, colour = .data$sensor)) +
    ggplot2::facet_grid(rows = vars(composition), cols = vars(temperature), scales = "free_y") +
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(x = -Inf, y = Inf, label = bottle, group = bottle),
      hjust = -0.5,
      vjust = 1.4,
      size = 3
    ) +
    ggplot2::scale_colour_manual(values = 1:40) +
    ggplot2::xlab("Day of Experiment") +
    ggplot2::ylab(expression("% O"[2])) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  p
}
