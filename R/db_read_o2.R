#' Read \code{o2} from RRD database and return as an \code{tbl_SQLiteConnection}.
#'
#' This function does not actually fetch the data,
#' but returns an \code{tbl_SQLiteConnection} object which can be further processed / filtered
#' using \code{dplyr}.
#' See \link{https://db.rstudio.com/r-packages/dplyr/} for details.
#' To fetch the actual data, execute \code{collect()}.
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param from_timestamp \code{integer}. Earliest timestamp to return. Default: \code{20210920}
#' @param to_timestamp \code{integer}. Latest timestamp to return. Default: \code{21000101}
#'
#' @return Link to the table \code{density} in the RRD to be used with \code{dplyr} and friends
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl
#'
#' @export
#'
#' @examples
db_read_o2 <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  from_timestamp = 20210920,
  to_timestamp = 21000101
){
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  data <- con %>%
    dplyr::tbl("o2") %>%
    filter (as.integer(timestamp) >= as.integer(from_timestamp)) %>%
    filter (as.integer(timestamp) <= as.integer(to_timestamp))
  return(data)
}
