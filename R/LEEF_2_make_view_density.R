#' Create \code{density} view wgich contains all density data from all measurements for LEEF-2
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param start_date the start date of the experiment
#' @param overwrite if \code{TRUE}, overwrite existing view
#'
#' @return the result of the execution of the ecreatiuon of the view.
#'
#' @export
#'
#' @examples
LEEF_2_make_view_density <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2022-11-07",
  overwrite = FALSE
){
  sql <- paste0(
"
CREATE VIEW density
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'bemovi_mag_16' AS measurement,
     species,
     density
   FROM
     bemovi_mag_16__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'bemovi_mag_25' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'bemovi_mag_25_cropped' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml_cropped
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'bemovi_mag_25_non_cropped' AS measurement,
     species,
     density
   FROM
     bemovi_mag_25__mean_density_per_ml_non_cropped
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'flowcam' AS measurement,
     species,
     density
   FROM
     flowcam__algae_density
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'flowcytometer' AS measurement,
     species,
     density
   FROM
     flowcytometer__flowcytometer_density
   UNION ALL
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     bottle,
     'manualcount' AS measurement,
     species,
     density
   FROM
     manualcount__manualcount_density
  )
INNER JOIN
  (
   SELECT
     bottle,
     incubator,
     temperature,
     resources,
     salinity,
     replicate
    FROM
	   experimetal_design
  )
USING
  (bottle)
"
  )

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
