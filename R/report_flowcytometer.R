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
#' @importFrom quarto quarto_render
#' @importFrom utils browseURL
#' @export
#'
#' @examples
report_flowcytometer <- function(
    timestamp = "20230106",
    extracted_base_dir = "~/Duck/LEEFSwift3/LEEF_2.archived.data/LEEF/3.archived.data/extracted/",
    output_dir = ".",
    format = "html"
) {
  name <- paste0("Flowcytometer_Report_", timestamp)

  report_name <- switch (
    format,
    html = {
      output_format <- "html"
      output_file <- paste0(name, ".html")
    },
    pdf = {
      output_format <- "pdf"
      output_file <-paste0(name, ".pdf")
    },
    word = {
      output_format <- "word"
      output_file <- paste0(name, ".docx")
    },
    stop("Unsupported format. Use 'html', 'pdf' or 'word'")
  )

  tmpdir <- tempfile()
  dir.create(tmpdir)
  template <-  file.path(tmpdir, "template.qmd")

  on.exit(unlink(tmpdir))

  file.copy(
    system.file("LEEF-2", "FlowcytometerGatingAssessmentReport.qmd", package = "LEEF.analysis"),
   template
  )

  report <- quarto::quarto_render(
    input = template,
    output_format = output_format,
    execute_params = list(timestamp = timestamp, extracted_base_dir = extracted_base_dir),
    output_file = output_file
  )

  file.copy(
    file.path(tmpdir, output_file),
    file.path(output_dir, output_file),
  )

  # utils::browseURL(report, encodeIfNeeded = TRUE)
  return(file.path(output_dir, output_file))
}
