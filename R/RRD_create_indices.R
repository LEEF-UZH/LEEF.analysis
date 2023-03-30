#' Create indices in RRD database
#'
#' @param dbname the path and name of the database. Must exist!
#' @param either "LEEF-1" or "LEEF-2"
#' @param contimue_after_error Do not quit execution of sql statements when error
#'   occurs but continue. \bold{Use with caution!} Default: \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
RRD_create_indices <- function(
    dbname,
    LEEF = NULL,
    continue_after_error = FALSE
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
      if (continue_after_error) {
        try(DBI::dbExecute(conn, s))
      } else {
        DBI::dbExecute(conn, s)
      }
    }
  )
  invisible(result)
}

