#' Plot diagnostic plot to check number of species per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return \code{ggplot} object of the plot
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot_no_species_per_bottle_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  config_yml = system.file("tables.yml", package = "LEEF.analysis")
){

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  on.exit({
    if (class(con) == "RSQLite") {
      DBI::dbDisconnect(con)
    }
  })

  sql <- '
SELECT
	"bemovi_mag_16" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM
	bemovi_mag_16__mean_density_per_ml
GROUP BY
	timestamp,
	bottle
UNION ALL
SELECT
	"bemovi_mag_25" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM
	bemovi_mag_25__mean_density_per_ml
GROUP BY
	timestamp,
	bottle
UNION ALL
SELECT
	"bemovi_mag_25" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM
	bemovi_mag_25__mean_density_per_ml_cropped
GROUP BY
	timestamp,
	bottle
UNION ALL
SELECT
	"flowcam" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM
	flowcam__algae_density
GROUP BY
	timestamp,
	bottle
UNION ALL
SELECT
	"flowcytometer" AS measurement,
	timestamp,
	bottle,
	COUNT(name) AS no_species
FROM
	flowcytometer__flowcytometer
GROUP BY
	timestamp,
	bottle
UNION ALL
SELECT
	"manualcount" AS measurement,
	timestamp,
	bottle,
	COUNT(species) AS no_species
FROM
	manualcount__manualcount
GROUP BY
	timestamp,
	bottle
  '

  data <- read_sql(db, sql = sql)
  data$timestamp <- as.Date(data$timestamp, "%Y%m%d")
  data$bottle[which(data$bottle == "b_100")] <- "b_c_1"
  data$bottle[which(data$bottle == "b_101")] <- "b_c_2"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$no_species)) +
    ggplot2::geom_point() +
    ggplot2::xlab("") +
    ggplot2::facet_wrap(~measurement)
  p
}
