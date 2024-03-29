#' Create new RRD database
#'
#' Create a new database following the scheme used for the LEEF.RRD database
#' @param dbname the path and name of the database. Must not exist.
#' @param either "LEEF-1" or "LEEF-2"
#' @param DBIDriver the DBI driver to use. Default is RSQLite::SQLite()
#'
#' @return
#' 
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom RSQLite SQLite
#' 
#' @export
#'
#' @examples
RRD_new <- function(
    dbname,
    LEEF = NULL,
    DBIDriver = RSQLite::SQLite()
) {
  if (file.exists(dbname)) {
    stop(
      "Database '", dbname, "' exists!\n",
      "  Please delete it before running this command again!"
    )
  }

  sql <- system.file(LEEF, "RRD.empty.sql", package = "LEEF.analysis")
  if (sql == "") {
    stop("The definition does not exist - specify 'LEEF-1' or 'LEEF-2' for 'LEEF'")
  }

  conn <- NULL
  conn <- DBI::dbConnect(DBIDriver, dbname)
  on.exit(
    try(
      DBI::dbDisconnect(conn),
      silent = TRUE
    )
  )

  sql <- readLines(sql)

  sql <- sql[!grepl("-- ", sql)]
  sql <- paste0(sql, collapse = " ")
  sql <- strsplit(sql, ";")

  result <- sapply(
    sql[[1]],
    function(s) {
      DBI::dbExecute(conn, s)
    }
  )
  invisible(result)
}
