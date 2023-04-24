#' Report for checking pipeline and sampling
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#' @param template Template to be used for report. At the moment only \code{"LEEF_1} and \code{"LEEF_2} supported.
#' @param suffix suffix for the file name
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.
#' @param lastDays number of last days to be included in graph 2. Default: 7
#'
#' @return the fully qualified file name to the created report.
#'
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @export
#'
#' @examples
report_diagnostic <- function(db, template = "LEEF_1", suffix = "", format = "html", lastDays = 7) {
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

  rmd <- switch (template,
    "LEEF_1" = system.file("LEEF-1", "DiagnosticReport.Rmd", package = "LEEF.analysis"),
    "LEEF_2" = stop("Pleaase use function `LEEF_2_report_diagnostic()` for LEEF-2!"),
    stop("Unsupprted report template!")
  )
  report <- rmarkdown::render(
    input = rmd,
    output_format = output_format,
    params = list(db = db, lastDays = lastDays),
    output_file = output_file
  )
  utils::browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
