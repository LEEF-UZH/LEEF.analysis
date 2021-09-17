#' Create \code{density} view wgich contains all density data from all measurements
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param overwrite if \code{TRUE}, overwrite existing view
#'
#' @return the result of the execution of the ecreatiuon of the view.
#'
#' @export
#'
#' @examples
make_view_density <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  overwrite = FALSE
){
  sql <- "
CREATE VIEW density
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp, bottle, 'bemovi_mag_16' AS measurement, species, density
   FROM
     bemovi_mag_16__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp, bottle,  'bemovi_mag_25' AS measurement, species, density
   FROM
     bemovi_mag_25__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp, bottle,  'bemovi_mag_25_cropped' AS measurement, species, density
   FROM
     bemovi_mag_25__mean_density_per_ml_cropped
   UNION ALL
   SELECT
     timestamp, bottle,  'flowcam' AS measurement, species, density
   FROM
     flowcam__algae_density
   UNION ALL
   SELECT
     timestamp, bottle,  'flowcytometer' AS measurement, species, density
   FROM
     flowcytometer__flowcytometer_density
   UNION ALL
   SELECT
     timestamp, bottle,  'manualcount' AS measurement, species, density
   FROM
     manualcount__manualcount_density
  )
INNER JOIN
  (
   SELECT
     'bottle',
     'temperature' AS temperature_treatment,
     'richness',
     'composition',
     'incubator'
   FROM
	 experimetal_design
  )
USING
  (bottle)
  "

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  if (overwrite) {
    DBI::dbExecute(
      con,
      "DROP VIEW density"
    )
  }
  DBI::dbExecute(
    con,
    sql
  )
}
