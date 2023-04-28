#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param treatment_begin_day begin of treatment (vertical red line in plot). If \code{NULL} none is plotted.
#' @param treatment_end_day end of treatment (vertical red line in plot). If \code{NULL} none is plotted.
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
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  treatment_begin_day = 70,
  treatment_end_day = 154
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
    if (!is.null(treatment_begin_day)) {
      p <- p + geom_vline(xintercept = range(treatment_begin_day), colour = "red")
    }
    if (!is.null(treatment_end_day)) {
      p <- p + geom_vline(xintercept = range(treatment_end_day), colour = "red")
    }
    p
  } else {
    warning("No data for available!")
  }
}
