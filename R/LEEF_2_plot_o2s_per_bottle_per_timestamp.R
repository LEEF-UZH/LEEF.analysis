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
LEEF_2_plot_o2s_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  treatment_begin_day = 70,
  treatment_end_day = NULL
){

  data <- db_read_o2(db) %>%
    dplyr::collect() %>%
    dplyr::mutate(percent_o2 = replace(percent_o2, percent_o2 == "---", NA)) %>%
    dplyr::mutate(percent_o2 = as.numeric(percent_o2)) %>%
    dplyr::mutate(sensor = as.character(sensor)) %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
    dplyr::mutate(bottle = fix_bottle(bottle))


  if (nrow(data) > 0) {
    data$label <- paste0("t: ", data$temperature, "\n r: ", data$resources, "\n s: ", data$salinity)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$day, y = .data$percent_o2)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$percent_o2, colour = .data$sensor)) +
      ggplot2::facet_grid(rows = vars(replicate), cols = vars(label),scales = "free_y") +
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
