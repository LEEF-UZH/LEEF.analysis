#' Render a quarto file in a temporary directory and copy the resulting files back to the working directory
#'
#' @param input the input qmd file
#' @param output_format the output format
#' @param ... additional arguments for \code{quarto::quarto_render()}
#'
#' @return
#'
#' @importFrom quarto quarto_render
#'
#' @examples
render_quarto_temp <- function(
    input,
    output_format = "html",
    ...
){
  name <- gsub("\\.qmd^", "", qmd)

  tmpdir <- tempfile()
  dir.create(tmpdir)

  on.exit(unlink(tmpdir))

  input_tmp <-  file.path(tmpdir, basename(qmd))

  file.copy(
    input,
    input_tmp
  )

  report <- quarto::quarto_render(
    input = input_tmp,
    output_format = output_format,
    ...
  )

  if (output_format == "word"){
    output_format <- "docx"
  }

  output_file_tmp <- gsub("\\.qrt^", paste0(".", output_format), input_tmp)
  output_folder <- file.path(output_dir)


  file.copy(
    output_file_tmp,
    ffn,
  )
}
