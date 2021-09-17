#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param measurement the measurement to be plotted. If \code{NULL},
#'   the default, they are plotted by temperature treatment (constant & increasing)
#' @param transform_density_4throot if \code{TRUE}, density is transformed using 4th root transformation.
#' Plot diagnostic plot to check bottles per timestamp
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
plot_bottles_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){
  density <- db_read_density(db) %>%
    dplyr::filter(timestamp == max(timestamp)) %>%
    dplyr::mutate(mxs = paste(species, measurement)) %>%
    dplyr::select(timestamp, bottle, composition, incubator, density, mxs)

  o2 <- db_read_o2(db) %>%
    dplyr::filter(timestamp == max(timestamp)) %>%
    dplyr::rename(density = percent_o2) %>%
    dplyr::mutate(mxs = measurement) %>%
    dplyr::select(timestamp, bottle, composition, incubator, density, mxs) %>%
    dplyr::collect() %>%
    dplyr::mutate(density = replace(density, density == "---", NA)) %>%
    dplyr::mutate(density = as.numeric(density))



  data <- dplyr::bind_rows(collect(density), o2)

  p <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$composition,
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
    ggplot2::facet_wrap(~mxs, ncol = 6, scales = "free",) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  p
}
