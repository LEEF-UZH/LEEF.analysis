#' Report for checking pipeline and sampling
#'
#' @param timestamp one or more timestamp to be plotted. If \code{NULL}, all
#'   timestamps available in \code{extracted_base_dir} will be plotted
#' @param extracted_base_dir directory in which the extracted data can be found
#'   with filenames as in the archive
#' @param leef LeEEF experiment, either \code{"LEEF-1"} or \code{"LEEF-2"}
#' @param output_dir output directory of the fil=nal report
#' @param format the format of the report as a character vector of length 1.
#'   Supported are at the moment: \code{html} (the default), \code{pdf} and
#'   \code{word}.
#' @param browse if \code{TRUE} (the default) opr=en the report in the
#'   appropriate program.
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
    leef = "LEEF-2",
    output_dir = ".",
    format = "html",
    browse = TRUE
) {
  if (is.null(timestamp)){
    timestamp <- switch (
      leef,
      "LEEF-1" = (
        list.files(extracted_base_dir, pattern = "flowcytometer") |>
          gsub(pattern = "LEEF\\.fast\\.flowcytometer\\.", replacement = "") |>
          unique() |>
          sort()
      ),
      "LEEF-2" = (
        list.files(extracted_base_dir, pattern = "flowcytometer") |>
          gsub(pattern = "LEEF\\.flowcytometer\\.flowcytometer\\.", replacement = "") |>
          unique() |>
          sort()
      ),
      stop("Unsupported `leef` argument. Only `LEEF-1` and `LEEF-2` are supported!")
    )
  }

  if (length(timestamp) > 1){
    fns <- lapply(
      timestamp,
      report_flowcytometer,
      extracted_base_dir = extracted_base_dir,
      leef = leef,
      output_dir = output_dir,
      format = format,
      browse = browse
    )
    return(fns)
  }

  template_file <- switch (
    leef,
    "LEEF-1" = system.file("LEEF-1", "Flowcytometer_Report.qmd", package = "LEEF.analysis"),
    "LEEF-2" = system.file("LEEF-2", "Flowcytometer_Report.qmd", package = "LEEF.analysis"),
    stop("Unsupported `leef` argument. Only `LEEF-1` and `LEEF-2` are supported!")
  )

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
    template_file,
    template
  )

  report <- quarto::quarto_render(
    input = template,
    output_format = output_format,
    execute_params = list(timestamp = timestamp, extracted_base_dir = extracted_base_dir),
  )

  dir.create(
    output_dir,
    showWarnings = FALSE,
    recursive = TRUE
  )

  ffn <- file.path(output_dir, output_file)

  file.copy(
    file.path(tmpdir, output_file),
    ffn,
  )

  if (browse){
    utils::browseURL(ffn, encodeIfNeeded = TRUE)
  }

  return(file.path(output_dir, output_file))
}
