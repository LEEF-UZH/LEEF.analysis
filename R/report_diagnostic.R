#' Report for checking pipeline and sampling
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.
#'
#' @return the fully qualified file name to the created report.
#'
#' @importFrom rmarkdown render
#' @export
#'
#' @examples
report_diagnostic <- function(db, format = "html") {
  report_name <- switch (
    format,
    html = {
      output_format <- "html_document"
      output_file <- file.path(dirname(db), "Diagnostic_report.html")
    },
    pdf = {
      output_format <- "pdf_document"
      output_file <- file.path(dirname(db), "Diagnostic_report.pdf")
    },
    word = {
      output_format <- "word_document"
      output_file <- file.path(dirname(db), "Diagnostic_report.docx")
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )
  report <- rmarkdown::render(
    input = system.file("DiagnosticPlotsForCheck.Rmd", package = "LEEF.analysis"),
    output_format = output_format,
    params = list(db = db),
    output_file = output_file
  )
  browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
