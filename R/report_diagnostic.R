#' Report for checking pipeline and sampling
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#'
#' @return the fully qualified file name to the created report.
#'
#' @importFrom rmarkdown render
#' @export
#'
#' @examples
report_diagnostic <- function(db) {
  report <- rmarkdown::render(
    input = system.file("DiagnosticPlotsForCheck.Rmd", package = "LEEF.analysis"),
    output_format = "html_document",
    params = list(db = db),
    output_file = file.path(dirname(db), "Diagnostic_plot.html")
  )
  browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
