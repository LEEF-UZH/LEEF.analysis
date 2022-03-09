#' Read a table from RRD database and return as an \code{tbl_SQLiteConnection}.
#'
#' This function does not actually fetch the data, but returns an
#' \code{tbl_SQLiteConnection} object which can be further processed / filtered
#' using \code{dplyr}. See \link{https://db.rstudio.com/r-packages/dplyr/} for
#' details. To fetch the actual data, execute \code{collect()}.
#' @param db fully qualified path to the sqlite database. Default, read from
#'   option \code{RRDdb}. If not set, defaults to option \code{RRDdb}; if this
#'   is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param table the name of the table. If `NULL`, a list of tables in the
#'   database `db` will be returned.
#' @param quiet if \code{TRUE} a warning will be issued whe the \code{table} name is NULL.
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
  table = NULL,
  quiet = FALSE
){
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)

  tables <- DBI::dbListTables(con)
  ##
  if (is.null(table)) {
    DBI::dbDisconnect(con)
  	if (!quiet){
  	  warning(
  	    "This function needs a table name.\n",
  	    "  Please specify a table by specifying `table = 'TABLENAME'` to read the table.\n",
  	    "  The table names are returned!"
  	  )
  	}
    data <- tables
  } else {
    if (!table %in% tables) {
      stop(
        table, " is not a table in the database.\n",
        "  The follwing tables are in the database:\n\n",
        paste(tables, collapse = "\n")
      )
    }
    data <- con %>%
      dplyr::tbl(table)
  }
  return(data)
}
