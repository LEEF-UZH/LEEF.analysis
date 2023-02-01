#' Read \code{stressor_levels} from RRD database and return as an \code{tbl_SQLiteConnection}.
#'
#' This function does not actually fetch the data,
#' but returns an \code{tbl_SQLiteConnection} object which can be further processed / filtered
#' using \code{dplyr}.
#' See \link{https://db.rstudio.com/r-packages/dplyr/} for details.
#' To fetch the actual data, execute \code{collect()}.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#'
#' @return Link to the table \code{stressor_levels} in the RRD to be used with \code{dplyr} and friends
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl
#'
#' @export
#'
#' @examples
db_read_stressor_levels <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite")
){
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  data <- dplyr::tbl(con, "stressor_levels")
  return(data)
}
