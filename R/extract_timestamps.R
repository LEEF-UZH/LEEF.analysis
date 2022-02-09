#' Extract data from table
#'
#' Extract data from `table` matching the `timestamps`.
#' For each `timestamp` the data will be saved as an `rds` file.
#' If `delete_data == TRUE` the data will be deleted from the database.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param table the name of the table. If `NULL`, a list of tables in the database `db` will be shown.
#' @param timestamps timestamps which should be extracted
#' @param delete_data **Attention!** If `TRUE` the data is deleted from the database!
#'
#' @return invisibly NULL
#' @export
#'
#' @md
#' @examples
extract_timestamps <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  table = NULL,
  timestamps,
  delete_data = FALSE
){
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  tables <- DBI::dbListTables(con)
  ##
  if (is.null(table)) {
    stop(
      "This function needs a table name. Please specify a table by specifying `table = 'TABLENAME'`\n",
      "  The follwing tables are in the database:\n\n",
      paste(tables, collapse = "\n")
    )
  }
  ##
  if (!table %in% tables) {
    stop(
      table, " is not a table in the database.\n",
      "  The follwing tables are in the database:\n\n",
      paste(tables, collapse = "\n")
    )
  }
  ##
  for (timestamp in timestamps) {
    message("Extracting ", timestamp, "...")
    timestamp <- as.character(timestamp)
    backup <- file.path(dirname(db), paste0("backup", ".", table, ".", timestamp, ".rds"))
    if (file.exists(backup)) {
      warning(
        "  Backup file ", backup, " exists!\n",
        timestamp, " NOT removed!"
      )
    } else {
      data <- con %>%
        dplyr::tbl(table) %>%
        dplyr::filter(timestamp == !!timestamp) %>%
        dplyr::collect()
      message("  Saving ", timestamp, "...")
      saveRDS(data, backup)
      if (delete_data){
        message("  Deleting ", timestamp, "...")
        delete <- paste0(
          "DELETE FROM ",
          table, " ",
          "WHERE ",
          "timestamp = '", timestamp, "'",
          ";"
        )
        DBI::dbExecute(
          con,
          delete
        )
      }
      message("Done")
    }
  }

  invisible(NULL)
}
