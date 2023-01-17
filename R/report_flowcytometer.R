#' Report for checking pipeline and sampling
#'
#' @param timestamp timestamp to be plotted
#' @param extracted_base_dir directory in which the extracted data can be found with filenames as in the archive
#' @param output_dir output directory of the fil=nal report
#' @param format the format of the report as a character vector of length 1.
#'     Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.
#' @param browse if \code{TRUE} (the default) opr=en the report in the appropriate program.
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
    format = "html",
    browse = TRUE
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
  template <-  file.path(tmpdir, paste0(name, ".qmd"))

  on.exit(unlink(tmpdir))

  file.copy(
    system.file("LEEF-2", "Flowcytometer_Report.qmd", package = "LEEF.analysis"),
    template
  )

  report <- quarto::quarto_render(
    input = template,
    output_format = output_format,
    execute_params = list(timestamp = timestamp, extracted_base_dir = extracted_base_dir),
  )

  ffn <- file.path(output_dir, output_file)
  file.copy(
    file.path(tmpdir, output_file),
    ffn,
  )

  utils::browseURL(ffn, encodeIfNeeded = TRUE)

  return(file.path(output_dir, output_file))
}
