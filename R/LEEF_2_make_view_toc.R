#' Create \code{toc} view wich contains selected fields from the toc__toc measurement for LEEF-2
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
LEEF_2_make_view_toc <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2022-11-07",
  overwrite = FALSE
){
  sql <- paste0(
"
CREATE VIEW toc
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast(
          julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) -
          julianday('", start_date, "') AS integer
     ) AS day,
     bottle,
     inj_type AS 'type',
	 conc AS 'concentration',
	 cv AS 'cv'
   FROM
     toc__toc
   WHERE
	 bottle IS NOT NULL
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
  (bottle);
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
      "DROP VIEW toc"
    )
  }
  DBI::dbExecute(
    con,
    sql
  )
}
