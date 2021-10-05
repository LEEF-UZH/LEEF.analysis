#' Plot diagnostic plot to check bottles per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return \code{ggplot} object of the plot
#'
#' @importFrom dplyr group_by summarise n collect mutate
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot_bottles_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  lastDays = 7
){
  data <- db_read_density(db) %>%
    dplyr::group_by(timestamp, species, bottle, measurement) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::collect() %>%
    dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
    dplyr::mutate(bottle = fix_bottle(bottle))

  data$exp_day <-  exp_day(data$timestamp)
  data <- data %>%
    dplyr::filter(exp_day <= lastDays)

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$timestamp, y = .data$bottle)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("") +
    ggplot2::facet_wrap(~measurement, ncol = 3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  p
}
