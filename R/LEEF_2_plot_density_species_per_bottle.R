#' Plot diagnostic plot to check number of species per timestamp for LEEF-2
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param transform_density_4throot if \code{TRUE}, density is transformed using 4th root transformation.
#' @param measurement the measurement to be plotted. If \code{NULL},
#'   the default, they are plotted by temperature treatment (constant & increasing)
#' @param species_set_id id of species set to use for filtering
#' @param treatment_begin_day begin of treatment (vertical red line in plot). If \code{NULL} none is plotted.
#' @param treatment_end_day end of treatment (vertical red line in plot). If \code{NULL} none is plotted.
#' @param arrow if \code{TRUE} read data from arrow instead of sqlite database
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
LEEF_2_plot_density_species_per_bottle_per_timestamp <- function(
    db = getOption("RRDdb", "LEEF.RRD.sqlite"),
    transform_density_4throot = TRUE,
    measurement = "bemovi_mag_16",
    species_set_id = NULL,
    treatment_begin_day = 70,
    treatment_end_day = 154,
  arrow = FALSE
){
  if (arrow) {
    density <- arrow_read_density()
    # o2 <- arrow_read_o2()
    # conductivity <- arrow_read_conductivity()
  } else {
    density <- db_read_density(db)
    # o2 <- db_read_o2(db)
    # conductivity <- db_read_conductivity(db)
  }
  options(dplyr.summarise.inform = FALSE)

  
  ## plotting
  spid <- species_set(species_set_id)
  data <- density %>%
    dplyr::filter(measurement == !!measurement)
  if (!is.null(species_set_id)) {
    data <- data |>
      dplyr::filter(
        species %in% species_set(species_set_id)
      )
  }
  data <- data |>
    dplyr::collect()
  
  if (nrow(data) < 1) {
    warning("No data for available!")
  } else {
    data <- data %>% dplyr::mutate(timestamp = convert_timestamp(timestamp)) %>%
      dplyr::mutate(bottle = fix_bottle(bottle)) %>%
      dplyr::mutate(
        density = if (transform_density_4throot) {
          exp(log(density)/4)
        } else {
          density
        }
      ) %>%
      group_by(day, bottle, species) %>%
      summarise(density = mean(density), temperature, resources, salinity, replicate)

    data$label <- paste0("t: ", data$temperature, "\n r: ", data$resources, "\n s: ", data$salinity)

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$day, y = .data$density)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$density, colour = .data$species)) +
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
      ggplot2::ylab(ifelse(transform_density_4throot, "4th root density", "density")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
    if (!is.null(treatment_begin_day)) {
      p <- p + geom_vline(xintercept = range(treatment_begin_day), colour = "red")
    }
    if (!is.null(treatment_end_day)) {
      p <- p + geom_vline(xintercept = range(treatment_end_day), colour = "red")
    }
    p
  }
}
