#' Read data from the RRD database
#'
#' The function executes the provided sql statement and returns the resulting table0.
#' For security, the database is opened in \bold{read-only} mode.!
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to \code{LEEF.RRD.sqlite}
#' @param sql sql statement
#'
#' @return the table resulting from the query as a \code{data.frame} object.
#'
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @export
#'
read_sql <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  sql
) {

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  on.exit({
    if (class(con) == "RSQLite"){
      DBI::dbDisconnect(con)
    }
  })

  table <- DBI::dbGetQuery(con, sql)

  ##

  return(table)
}
