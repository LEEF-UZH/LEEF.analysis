#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param type the type of the measurement which should be displayed. A vector with the types.
#'   Possible values are: "TOC", "TN", "IC", "TN", "".
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
LEEF_2_plot_tocs_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  type = c("IC", "TC", "TN", "TOC")
){
  options(dplyr.summarise.inform = FALSE)

  data <- db_read_toc(db) %>%
    filter(type %in% !!type) %>%
    dplyr::collect()

  if (nrow(data) < 1) {
    warning("No data for available!")
  } else {
    data <- data %>%
      dplyr::mutate(bottle = fix_bottle(bottle)) %>%
      # dplyr::mutate(
      #   density = if (transform_density_4throot) {
      #     exp(log(density)/4)
      #   } else {
      #     density
      #   }
      # ) %>%
      group_by(day, bottle, type, salinity, resources, temperature) # %>%
    # summarise(density = mean(density))

    data$temperature[data$temperature == "increasing"] <- "decreasing light"
    data$temperature[data$temperature == "constant"]   <- "constant light"

    ls <- db_read_light_decline(db) %>% collect()

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$day, y = .data$concentration)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$concentration, colour = .data$type)) +
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
      ggplot2::ylab("concentration") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) +
      geom_vline(xintercept = range(ls$day), colour = "lightgrey")
    p
  }
}
