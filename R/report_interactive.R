#' Run interactive report
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#'
#' @return invisibkly \code{NULL}
#'
#' @importFrom rmarkdown run
#' @export
#'
#' @examples
report_interactive <- function(db) {
  rmarkdown::run(
    file = system.file("InteractiveReport.Rmd", package = "LEEF.analysis")
  )
  return(NULL)
}
