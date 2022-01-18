#' Add data to RRD database into existing table
#'
#' @param fn `rds` file containing data to be added. The name has to be as expected for the additor function.
#' @param db RRD sqlite databaes
#' @param table name of the table the data should be added to
#'
#' @return `TRUE` if the data has been added, `FALSE` otherwise
#' @export
#'
#' @md
#' @examples
add_to_db <- function(
  fn,
  dbname,
  table
){
  added <- FALSE

  dat <- readRDS(fn)
  names(dat) <- tolower(names(dat))

  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbname
  )
  on.exit({
    try(DBI::dbDisconnect(conn), silent = TRUE)
  })


  timestamps <- unlist(
    DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", table))
  )
  if (any(unique(dat$timestamp) %in% timestamps)) {
    warning("'", fn, "' not added as timestamp already present in table '", table, "'.")
    added <- FALSE
  } else {
    DBI::dbBegin(conn)
    DBI::dbWriteTable(
      conn,
      name = table,
      value = dat,
      overwrite = FALSE,
      append = TRUE
    )
    DBI::dbCommit(conn)
    DBI::dbExecute(conn, "VACUUM")
    added <- TRUE
  }

  return(added)
}
