#' Report for the sanity check of the data for submission to the pipeline
#'
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{0.raw.data}
#'   are located
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
report_sanity_check <- function(sample_dir, suffix = "", format = "html") {
  name <- ifelse(
    suffix == "",
    "Sanity_Check_report",
    paste0("Sanity_Check_report.", suffix)
  )

  sample_dir <- dirname(file.path(sample_dir, "."))
  report_name <- switch (
    format,
    html = {
      output_format <- "html_document"
      output_file <- file.path(sample_dir, paste0(name, ".html"))
      output_dir = sample_dir
    },
    pdf = {
      output_format <- "pdf_document"
      output_file <- file.path(sample_dir, paste0(name, ".pdf"))
      output_dir = sample_dir
    },
    word = {
      output_format <- "word_document"
      output_file <- file.path(sample_dir, paste0(name, ".docx"))
      output_dir = sample_dir
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )
  report <- rmarkdown::render(
    input = system.file("SanityCheckReport.Rmd", package = "LEEF.analysis"),
    output_format = output_format,
    params = list(sample_dir = sample_dir),
    output_file = output_file
  )
  utils::browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
