#' Create all views
#'
#' This function calls the other \code{make_view_...()} functions, wrapped in individual \code{try()} blocks.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param start_date the start date of the experiment
#' @param overwrite if \code{TRUE}, overwrite existing view
#'
#' @return invisibly \code{NULL}
#'
#' @export
#'
#' @examples
make_views <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2021-09-20",
  overwrite = FALSE
){
  try(make_view_density(db = db, start_date = start_date, overwrite = overwrite))
  try(make_view_o2(db = db, start_date = start_date, overwrite = overwrite))
  try(make_view_toc(db = db, start_date = start_date, overwrite = overwrite))
  try(make_view_conductivity(db = db, start_date = start_date, overwrite = overwrite))
  invisible(NULL)
}
