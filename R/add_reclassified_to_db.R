#' Title
#'
#' @param path path to the root directory of the reclassification
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param remove_timestamps vector of timestamps to be removed.
#' @param check_timestamps. If `TRUE`, the data will ony be added when timestamp does not exist in db yet. If `FALSE`,
#'   it will always be added. Usually this should **NOT** be done.#'
#' @return
#' @export
#'
#' @examples
add_reclassified_to_db <- function(
  path,
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  remove_timestamps = NULL,
  check_timestamps = TRUE
){
  if (!file.exists(db)){
    LEEF.backend.sqlite::new_RRD(db)
  }

  methods <- list.dirs(
    path,
    recursive = FALSE
  )

  for (method in methods) {
    files <- list.files(method, full.names = TRUE)
    files <- grep("trajectories", files, invert = TRUE, value = TRUE)
    tables <- paste0(
      basename(method), "__",
      sapply( files, function(f) {strsplit(basename(f), "\\.")[[1]][[1]]})
    )
    names(tables) <- NULL

    message("Adding directory '", method, "' to the database")
    add_to_db(
      fns = files,
      tables = tables,
      db = db,
      remove_timestamps = remove_timestamps,
      check_timestamps = check_timestamps
    )
  }
}
