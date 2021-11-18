#' Read a tablefrom RRD database and return as an \code{tbl_SQLiteConnection}.
#'
#' This function does not actually fetch the data,
#' but returns an \code{tbl_SQLiteConnection} object which can be further processed / filtered
#' using \code{dplyr}.
#' See \link{https://db.rstudio.com/r-packages/dplyr/} for details.
#' To fetch the actual data, execute \code{collect()}.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return Link to the table \code{density} in the RRD to be used with \code{dplyr} and friends
#'
#' @importFrom DBI dbConnect dbListTables dbDisconnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl
#'
#' @export
#'
#' @examples
db_read_table <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  table = NULL
){
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  tables <- DBI::dbListTables(con)
  ##
  if (is.null(table)) {
    DBI::dbDisconnect(con)
  	stop(
  	  "This function needs a table name. Please specify a table by specifying `table = 'TABLENAME'`\n",
  	  "  The follwing tables are in the database:\n\n",
  	  paste(tables, collapse = "\n")
  	)
  }
  if (!table %in% tables) {
    stop(
      table, " is not a table in the database.\n",
      "  The follwing tables are in the database:\n\n",
      paste(tables, collapse = "\n")
    )
  }
  data <- con %>%
    dplyr::tbl(table)
  return(data)
}
