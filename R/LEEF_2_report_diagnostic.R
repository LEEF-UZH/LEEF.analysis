#' Report for checking pipeline and sampling
#'
#' @param db fully qualified path to the sqlite database. The report will be saved to the same directory.
#' @param suffix suffix for the file name
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.
#' @param lastDays number of last days to be included in graph 2. Default: 7
#' @param arrow if \code{TRUE} read data from arrow instead of sqlite database
#' @param parquet_dir directory where the parquet files are stored. Default: '/Volumes/RRD.Reclassification_LEEF-2/parquet/'. Will only be used whe \code{arrow = TRUE}
#'
#' @return the fully qualified file name to the created report.
#'
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @export
#'
#' @examples
LEEF_2_report_diagnostic <- function(
    db = NULL,
    suffix = "",
    format = "html",
    lastDays = 7,
    arrow = FALSE,
    parquet_dir = "/Volumes/RRD.Reclassification_LEEF-2/parquet/") {
  name <- ifelse(
    suffix == "",
    "Diagnostic_Report",
    paste0("Diagnostic_Report.", suffix)
  )

  output <- ifelse(
    arrow,
    file.path(parquet_dir, "..", name),
    file.path(dirname(db), name)
  )
  report_name <- switch(format,
    html = {
      output_format <- "html_document"
      output_file <- paste0(output, ".html")
      output_dir <- output
    },
    pdf = {
      output_format <- "pdf_document"
      output_file <- paste0(output, ".pdf")
      output_dir <- output
    },
    word = {
      output_format <- "word_document"
      output_file <- paste0(output, ".docx")
      output_dir <- output
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )
  report <- rmarkdown::render(
    input = system.file("LEEF-2", "DiagnosticReport.Rmd", package = "LEEF.analysis"),
    output_format = output_format,
    params = list(db = db, lastDays = lastDays, arrow = arrow, parquet_dir = parquet_dir),
    output_file = output_file
  )
  utils::browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
