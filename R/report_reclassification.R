#' Do the reclassification and create a report
#'
#' The report needs to adapted to new classifications.
#' @param reclassification_report The name of the reclassification report. The default points to the first one.
#'   Included reclassification reports are:
#'   - `system.file(Reclassification_2022.01.10.Rmd, package = "LEEF.analysis")`
#'   - `system.file(Reclassification_20220105_20220221_flowcam.Rmd, package = "LEEF.analysis")`
#'   - `system.file(Reclassification_20220506.Rmd, package = "LEEF.analysis")`
#' @param leef LEEF study - allowed are `"LEEF-1` and `"LEEF-2"`
#' @param output_dir directory of the output of the reclassification and the report
#' @param output_name The name of the final report, without extension.
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: `html` (the default), `pdf` and `word`.
#'
#' @return the fully qualified file name to the created report.
#'
#' @md
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @export
#'
#' @examples
report_reclassification <- function(
  reclassification_report = system.file(leef, "Reclassification_20220105.Rmd", package = "LEEF.analysis"),
  leef = "LEEF-1",
  output_dir =  normalizePath(file.path("~", "Reclassification_20220105")),
  output_name = "Reclassification_20220105",
  format = "html"
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  report_name <- switch (
    format,
    html = {
      output_format <- "html_document"
      output_file <- file.path(output_dir, paste0(output_name, ".html"))
    },
    pdf = {
      output_format <- "pdf_document"
      output_file <- file.path(output_dir, paste0(output_name, ".pdf"))
    },
    word = {
      output_format <- "word_document"
      output_file <- file.path(output_dir, paste0(output_name, ".docx"))
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )

  report <- rmarkdown::render(
    input = reclassification_report,
    output_format = output_format,
    output_file = output_file,
    params = list(output_dir = output_dir),
  )

  utils::browseURL(report, encodeIfNeeded = TRUE)
  return(report)
}
