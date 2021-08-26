#' Add \code{composition} and \code{experimetal_design} table to db.
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param composition fully qualified path to the \code{csv} file with the composition data
#' @param experimetal_design fully qualified path to the \code{csv} file with the experimetal_design data
#' @param overwrite if \code{TRUE}, the existing tables will be overwritten. There is actually no need for this.
#'
#' @importFrom utils read.csv
#' @return NULL
#' @export
#'
#' @examples
add_experiment_tables <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  composition = NULL,
  experimetal_design = NULL,
  overwrite = FALSE
) {
  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  if (!is.null(composition)) {
    if (!file.exists(composition)) {
      stop("'compsition'ad_ must poiint to an existing csv file!")
    }
#    if (!DBI::dbExistsTable(con, "composition")) {
      DBI::dbWriteTable(
        con,
        name = "composition",
        value = utils::read.csv(composition),
        overwrite = overwrite,
        append = FALSE
      )
#    } else {
#      message("Table 'composition' exists in database. To overwrite specify 'overwrite = TRUE'")
#    }
  }

  if (!is.null(experimetal_design)) {
    if (!file.exists(experimetal_design)) {
      stop("'experimetal_design' must poiint to an existing csv file!")
    }
#    if (!DBI::dbExistsTable(con, "experimetal_design")) {
      DBI::dbWriteTable(
        con,
        name = "experimetal_design",
        value = utils::read.csv(experimetal_design),
        overwrite = overwrite,
        append = FALSE
      )
#    } else {
#      message("Table 'experimetal_design' exists in database. To overwrite specify 'overwrite = TRUE'")
#    }
  }
}
