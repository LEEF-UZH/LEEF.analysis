#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return \code{ggplot} object of the plot
#'
#' @importFrom dplyr group_by summarise n collect mutate filter
#' @import ggplot2
#'
#' @export
#'
#' @examples
LEEF_2_plot_response_incubator <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){
  density <- db_read_density(db) %>%
    dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
    dplyr::mutate(mxs = paste(species, measurement)) %>%
    dplyr::mutate(treatment = paste0("t: ", temperature, "; r: ", resources, "; s: ", salinity)) %>%
    dplyr::select(timestamp, bottle, temperature, resources, treatment, incubator, density, mxs) %>%
    collect()

  o2 <- db_read_o2(db) %>%
    dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
    dplyr::rename(density = percent_o2) %>%
    dplyr::mutate(mxs = measurement) %>%
    dplyr::mutate(treatment = paste0("t: ", temperature, "; r: ", resources, "; s: ", salinity)) %>%
    dplyr::select(timestamp, bottle, treatment, incubator, density, mxs) %>%
    dplyr::collect() %>%
    dplyr::mutate(density = replace(density, density == "---", NA)) %>%
    dplyr::mutate(density = as.numeric(density))

  conductivity <- db_read_conductivity(db) %>%
    dplyr::filter(timestamp == max(timestamp, na.rm = TRUE)) %>%
    dplyr::rename(density = conductivity) %>%
    dplyr::mutate(mxs = measurement) %>%
    dplyr::mutate(treatment = paste0("t: ", temperature, "; r: ", resources, "; s: ", salinity)) %>%
    dplyr::select(timestamp, bottle, treatment, incubator, density, mxs) %>%
    dplyr::collect() %>%
    dplyr::mutate(density = replace(density, density == "---", NA)) %>%
    dplyr::mutate(density = as.numeric(density))


  # if(nrow(density) == 0 & nrow(o2) == 0){
  #   data <- NULL
  # } else if (nrow(density) == 0){
  #   data <- o2
  # } else if (nrow(o2) == 0) {
  #   data <- density
  # } else {
  #   data <- dplyr::bind_rows(collect(density), o2)
  # }
  data <- dplyr::bind_rows(collect(density), o2, conductivity)

  if (!is.null(data)){
    p <- data %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$treatment,
          y = .data$density,
          colour = .data$incubator,
          shape = .data$incubator
        )
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "A" = "#00798c",
          "B" = "#edae49",
          "C" = "#66a182",
          "D" = "#2e4057",
          "E" = "red",
          "F" = "#8d96a3",
          "G" = "#d1495b",
          "H" = "#8d96a3"
        )
      ) +
      ggplot2::scale_shape_manual(
        values = c(
          "A" = 20,
          "B" = 20,
          "C" = 20,
          "D" = 20,
          "E" = 23,
          "F" = 20,
          "G" = 20,
          "H" = 20
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::facet_wrap(~mxs, ncol = 3, scales = "free_y",) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, debug = FALSE))
    p
  } else {
    warning("No data available!")
  }
}
