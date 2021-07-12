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
