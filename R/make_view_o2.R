#' Create \code{o2} view wgich contains selected fields from the os measurement
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
make_view_o2 <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  overwrite = FALSE
){
  sql <- "
CREATE VIEW o2
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     bottle,
     sensor,
     temp AS 'temperature_actual',
     value AS 'percent_o2',
     'o2meter' AS measurement
   FROM
     o2meter__o2meter
  )
INNER JOIN
  (
   SELECT
     bottle,
     temperature,
     richness,
     composition,
     incubator
   FROM
	 experimetal_design
  )
USING
  (bottle);
  "

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  if (overwrite) {
    DBI::dbExecute(
      con,
      "DROP VIEW o2"
    )
  }
  DBI::dbExecute(
    con,
    sql
  )
}
