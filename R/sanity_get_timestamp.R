#' Extract timestamp from \code{sample_metadata.yml} file
#'
#' @param sample_dir root dir in which the folder\code{00.general.parameter} and \code{9.raw,data}
#'   are located
#'
#' @return The extracted timestamp as a string
#' @export
#'
#' @examples
sanity_get_timestamp <- function(
  sample_dir = "."
){
  file <- file.path(sample_dir, "00.general.parameter", "sample_metadata.yml")
  timestamp <- yaml::read_yaml(file)$timestamp
  return(timestamp)
}

#' Sanity check for o2meter
#'
#' @param sample_dir root dir in which the folder\code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
#' @examples
sanity_check_o2meter <- function(
  sample_dir = "."
){
  timestamp <- get_timestamp(sample_dir)
  fd <- file.path(sample_dir, "0.raw.data", "o2meter")
  fn <- list.files(fd)

  if (length(fn) != 0) {
    return("Either o2meter file is missing or more than one file exists. Exactly one expected!")
  }

  mod_time <- format(x = file.mtime(fn), "%Y%m%d")
  if (mod_time != timestamp) {
    return("The timestamp is not equal to the modification time of the o2meter file!")
  }

  return(TRUE)
}

#' Sanity check for manualcount
#'
#' @param sample_dir root dir in which the folder\code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
#' @examples
sanity_check_manualcount <- function(
  sample_dir = "."
){
  timestamp <- get_timestamp(sample_dir)
  fd <- file.path(sample_dir, "0.raw.data", "manualcount")
  fn <- list.files(fd)

  if (length(fn) != 0) {
    return("Either manualcount file is missing or more than one file exists. Exactly one expected!")
  }

  if (basename(fn)[[1]] != "manual_count.xlsx") {
    return("Wrong file name. The file has to be named 'manual_count.xlsx'!")
  }

  mod_time <- format(x = file.mtime(fn[[1]]), "%Y%m%d")
  if (mod_time != timestamp) {
    return("The timestamp is not equal to the modification time of the o2meter file!")
  }
  return(TRUE)
}



