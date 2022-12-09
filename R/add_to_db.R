#' Add data to RRD database into existing table
#'
#' @param fns vector of `rds` or `csv` files names containing data to be added.
#'   The data has to contain a colum named `timestamp`.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param tables `vector` of the table names the data of the `fns` of
#'   the same index should be added to.
#'   **The table has to exist and contain a field named `timestamp`**
#' @param remove_timestamps vector of timestamps to be removed.
#' @param check_timestamps. If `TRUE`, the data will ony be added when timestamp does not exist in db yet. If `FALSE`,
#'   it will always be added. Usually this should **NOT** be done.
#'   @param backup_removed if 'TRUE` data which will be replaced will be backed up.
#'
#' @return vector of length of `fns` with `TRUE` if the data has been added,
#'   `FALSE` otherwise
#' @export
#'
#' @md
#' @examples
add_to_db <- function(
  fns,
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  tables,
  remove_timestamps = NULL,
  check_timestamps = TRUE,
  backup_removed = TRUE
){
  if (length(fns) != length(tables)){
    stop("'fns' and 'tables' have to have the same length!")
  }

  added <- FALSE

  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db = db
  )
  on.exit({
    try(DBI::dbDisconnect(conn), silent = TRUE)
  })

  if (!is.null(remove_timestamps)){
    tss <- unique(remove_timestamps)
    sapply(
      unique(tables),
      function(table){
        message("Removing timestamps from ", table)
        extract_timestamps(
          db = db,
          table = table,
          timestamps = tss,
          delete_data = TRUE,
          only_delete = !backup_removed
        )
      }
    )
  }

  added <- sapply(
    1:length(fns),
    function(i){
      message("Adding '", basename(fns[i]), "' to '", tables[i], "'..."  )

      if (grepl("\\.rds$", fns[i])) {
        dat <- readRDS(fns[i])
      } else if (grepl("\\.csv$", fns[i])) {
        dat <- read.csv(fns[[i]])
      } else {
        stop("Input file ", fns[[i]], " has to be either `.csv` or `.rds`!")
      }
      names(dat) <- tolower(names(dat))
      if ("timestamp" %in% names(dat)) {
        dat$timestamp <- as.character(dat$timestamp)
      }

      if (check_timestamps) {
        timestamps <- unlist(
          DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", tables[i]))
        )

        if (any(unique(dat$timestamp) %in% timestamps)) {
          msg <- paste0("'", fns[i], "' not added as timestamp already present in table '", tables[i], "'.")
          message(msg)
          warning(msg)
          return <- FALSE
        }
      }
      DBI::dbBegin(conn)
      DBI::dbWriteTable(
        conn,
        name = tables[i],
        value = as.data.frame(dat),
        overwrite = FALSE,
        append = TRUE
      )
      DBI::dbCommit(conn)
      return(TRUE)
    }
  )

  return(added)
}
