#' Create \code{light_decline} view which adds the experimental day
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param start_date the start date of the experiment
#' @param overwrite if \code{TRUE}, overwrite existing view
#'
#' @return the result of the execution of the creation of the view.
#'
#' @export
#'
#' @examples
make_view_light_decline <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2021-09-20",
  overwrite = FALSE
){
  sql <- paste0(
"
CREATE VIEW light_decline
AS
SELECT
  *
FROM
  (
   SELECT
     timestamp,
     cast( julianday(date(substr(timestamp,1,4)||'-'||substr(timestamp,5,2)||'-'||substr(timestamp,7,2))) - julianday('", start_date, "') as integer ) AS day,
     temperature,
     light
   FROM
     light_decline_schedule
  )
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
      "DROP VIEW light_decline"
    )
  }
  DBI::dbExecute(
    con,
    sql
  )
}
