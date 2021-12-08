#' Create trimmed down density-only version of the database
#'
#' This function creates a trimmed down version of the database which
#' does not contain the tables only necessary for the classification.
#' It does not do anything with the original database.
#' @param db fully qualified path to the sqlite database. Default, read
#'   from option \code{RRDdb}. If not set, defaults to option \code{RRDdb};
#'   if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param trimmed_db fully qualified path to the trimmed sqlite database.
#'   Defaults to the db with the extension `trimmed.sqlite` instead of `sqlite`.
#'
#' @return the path and name of the trimmed database
#'
#' @md
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl
#'
#' @export
#'
#' @examples
create_rrd_trimmed <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  trimmed_db = gsub("\\.sqlite", ".trimmed.sqlite", db)
){
  if (file.exists(trimmed_db)) {
    stop("The trimmed_db exists.\nPlease remove it by hand.\nThis function will not overwrite anything!")
  }
  tmpdb <- tempfile(fileext = ".sqlite")
  ##
  con <- NULL
  done <- FALSE
  on.exit(
    {
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
      }
      if (!done) {
        unlink(trimmed_db)
      }
    }
  )
  ##
  message("Copying to temporary location - this might take a long time!")
  file.copy(
    from = db,
    to = tmpdb
  )
  ##
  to_drop <- c(
    "bemovi_mag_16__morph_mvt",
    "bemovi_mag_25__master_non_cropped",
    "bemovi_mag_25__morph_mvt",
    "bemovi_mag_25__morph_mvt_cropped",
    "bemovi_mag_25__morph_mvt_non_cropped",
    "flowcam__algae_traits"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), tmpdb)
  for (tbl in to_drop) {
    message("Dropping table ", tbl)
    DBI::dbExecute(
      con,
      paste0("DROP TABLE ", tbl)
    )
  }
  message("Vacuuming")
  DBI::dbExecute(
    con,
    "VACUUM"
  )
  DBI::dbDisconnect(con)
  message("Moving to finak location")
  file.rename(
    from = tmpdb,
    to = trimmed_db
  )
  con <- NULL
  done <- TRUE
  message("Done")
  return(trimmed_db)
}
