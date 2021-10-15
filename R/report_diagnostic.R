#' Report for checking pipeline and sampling
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#' @param suffix suffix for the file name
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.
#'
#' @return the fully qualified file name to the created report.
#'
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @export
#'
#' @examples
report_diagnostic <- function(db, suffix = "", format = "html") {
  name <- ifelse(
    suffix == "",
    "Diagnostic_Report",
    paste0("Diagnostic_Report.", suffix)
  )

  report_name <- switch (
    format,
    html = {
      output_format <- "html_document"
      output_file <- file.path(dirname(db), paste0(name, ".html"))
      output_dir = dirname(db)
    },
    pdf = {
      output_format <- "pdf_document"
      output_file <- file.path(dirname(db), paste0(name, ".pdf"))
      output_dir = dirname(db)
    },
    word = {
      output_format <- "word_document"
      output_file <- file.path(dirname(db), paste0(name, ".docx"))
      output_dir = dirname(db)
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )
  report <- rmarkdown::render(
    input = system.file("DiagnosticReport.Rmd", package = "LEEF.analysis"),
    output_format = output_format,
    params = list(db = db),
    output_file = output_file
  )
  utils::browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
