#' Disconnect the \code{tbl_SQLiteConnection}
#'
#' Disconnect the object returned by the \code{db_read_...} functions.
#' @param x a \code{tbl_SQLiteConnection} object
#'
#' @return invisibly \code{NULL}
#'
#' @importFrom DBI dbConnect
#'
#' @export
#'
db_disconnect <- function(
  x
){
  if (!inherits(x, "tbl_SQLiteConnection")) {
    stop("x is not a 'tbl_SQLiteConnection'!")
  } else {
    DBI::dbDisconnect(x$src$con)
  }
  invisible(NULL)
}
