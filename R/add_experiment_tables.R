#' Add \code{composition} and \code{experimetal_design} table to db.
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param composition fully qualified path to the \code{csv} file with the composition data
#' @param composition fully qualified path to the \code{csv} file with the composition data
#' @param light_decline_schedule fully qualified path to the \code{csv} file with the light decline dates
#' @param immigration_schedule fully qualified path to the \code{csv} file with the immigration event dates
#' @param overwrite if \code{TRUE}, the existing tables will be overwritten. There is actually no need for this.
#'
#' @importFrom utils read.csv
#' @return NULL
#' @export
#'
add_experiment_tables <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  composition = NULL,
  experimetal_design = NULL,
  light_decline_schedule = NULL,
  immigration_schedule = NULL,
  overwrite = FALSE
) {

  addtbl <- function(con, csv, tbl){
    if (!is.null(csv)) {
      if (!file.exists(csv)) {
        stop("'", csv, "'ad_ must point to an existing csv file!")
      }
      DBI::dbWriteTable(
        con,
        name = tbl,
        value = utils::read.csv(csv),
        overwrite = overwrite,
        append = FALSE
      )
    }
  }

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
  })

  addtbl(con, composition, "composition")
  addtbl(con, experimetal_design, "experimetal_design")
  addtbl(con, light_decline_schedule, "light_decline_schedule")
  addtbl(con, immigration_schedule, "immigration_schedule")

}
