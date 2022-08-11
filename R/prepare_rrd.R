#' Prepare the RRD database for usage with the other functions in this package.
#'
#' This function adds the needed tables and creates the vuews needed for the analysis.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param composition fully qualified path to the \code{csv} file with the composition data
#' @param experimetal_design fully qualified path to the \code{csv} file with the experimetal_design data
#' @param overwrite if \code{TRUE}, the existing tables / views will be overwritten.
#'
#' @return NULL
#' @export
#'
prepare_rrd <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  composition = NULL,
  experimetal_design = NULL,
  overwrite = FALSE
){
  add_experiment_tables(db = db, composition = composition, experimetal_design = experimetal_design, overwrite = overwrite)
  make_view_density(db = db, overwrite = overwrite)
  make_view_o2(db = db, overwrite = overwrite)
}
