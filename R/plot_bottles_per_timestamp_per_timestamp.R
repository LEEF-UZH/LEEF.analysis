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
plot_bottles_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  lastDays = 7
){
  density <- db_read_density(db) %>%
    dplyr::select(timestamp, day, bottle, measurement) %>%
    dplyr::filter(day >= (max(day, na.rm=TRUE) - lastDays)) %>%
    # dplyr::group_by(timestamp, day, bottle, measurement) %>%
    # dplyr::summarise(n = dplyr::n()) %>%
    dplyr::collect()

  o2 <- db_read_o2(db) %>%
    dplyr::select(timestamp, day, bottle, measurement) %>%
    dplyr::filter(day >= (max(day, na.rm=TRUE) - lastDays)) %>%
    # dplyr::group_by(timestamp, day, bottle, measurement) %>%
    # dplyr::summarise(n = dplyr::n()) %>%
    dplyr::collect()

  if(nrow(density) == 0 & nrow(o2) == 0){
    data <- NULL
  } else if (nrow(density) == 0){
    data <- o2
  } else if (nrow(o2) == 0) {
    data <- density
  } else {
    data <- dplyr::bind_rows(collect(density), o2)
  }

  if (is.null(data)) {
    warning("No data available!")
  } else {
    max_day <- max(data$day) - lastDays
    data <- data %>%
      dplyr::filter(day >= max_day) %>%
      dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
      dplyr::mutate(bottle = fix_bottle(bottle))

    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$timestamp, y = .data$bottle)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::facet_wrap(~measurement, ncol = 3) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
    p
  }
}
