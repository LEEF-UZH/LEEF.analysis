#' Create indices in RRD database
#'
#' @param dbname the path and name of the database. Must exist!
#' @param either "LEEF-1" or "LEEF-2"
#'
#' @return
#' @export
#'
#' @examples
RRD_create_indices <- function(
    dbname,
    LEEF = NULL
){
  if (!file.exists(dbname)) {
    stop(
      "Database '", dbname, "' does not exist!\n")
  }

  sql <- system.file(LEEF, "RRD.indices.sql", package = "LEEF.analysis")
  if (sql == ""){
    stop("The definition does not exist - specify 'LEEF-1' or 'LEEF-2' for 'LEEF'")
  }

  conn <- NULL
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname )
  on.exit(
    try(
      DBI::dbDisconnect(conn),
      silent = TRUE
    )
  )

  sql <- readLines(sql)
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

