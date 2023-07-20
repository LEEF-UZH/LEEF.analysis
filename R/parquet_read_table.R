#' Open Arrow DB with parquet files
#'
#' Opens the arrow dataset of parquet files. The resulting pbject can be used in a dplyr pipeline as a starting point.
#' @param def Path to the parquet files definition.
#'
#' @return Link to the table \code{density} in the RRD to be used with \code{dplyr} and friends
#'
#' @importFrom arrow open_dataset
#'
#' @export
#'
#' @examples
parquet_open_table <- function(
  def
  ){
  dataset <- open_dataset(
    sources = def$sources,
    partitioning = def$partitioning,
    hive_style = def$hive_style,
  )

  return(dataset)
}
