#' Open Arrow DB with parquet files
#'
#' Opens the arrow dataset of parquet files. The resulting pbject can be used in a dplyr pipeline as a starting point.
#' @param def Path to the parquet files definition or the actual definition object. These have usually the name `TABLENAME.sources.rds`
#'
#' @return Object to the table \code{density} in the RRD to be used with \code{dplyr} and friends
#'
#' @importFrom arrow open_dataset
#'
#' @export
#'
#' @examples
parquet_open_table <- function(def) {
  if (is.character(def)){
    def <- readRDS(def)
  }

  # should later be added
  if (inherits(new_parquet_definition(), "parquet_definition")) {
    warning("The opbject `def` or the object stored at `def` is not a parquet_definition object. This might lead to unexpected behaviour.")
  }
  
  dataset <- open_dataset(
    sources = def$sources,
    partitioning = def$partitioning,
    hive_style = def$hive_style
  )

  return(dataset)
}
