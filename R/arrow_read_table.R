#' Read a table from an arrow database and return as an Dataset object.
#'
#' This function does not actually fetch the data, but returns an
#' Dataset object which can be further processed / filtered
#' using \code{dplyr}. See \link{https://db.rstudio.com/r-packages/dplyr/} for
#' details. To fetch the actual data, execute \code{collect()}.
#' This function is simply a convenience and consistency wrapper around \code{\link[arrow]{open_dataset}}
#' @param table Directory of the data files
#' @param db fully qualified path to the folder which contains all the arrow directories, Defaul: \code{getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/")}
#' @param hive_style see \code{\link[arrow]{open_dataset}}
#' @param unify_schemas see \code{\link[arrow]{open_dataset}}
#' @param format see \code{\link[arrow]{open_dataset}}
#'
#' @return A Dataset R6 object. Use dplyr methods on it to query the data.
#'
#' @importFrom arrow open_dataset
#'
#' @export
#'
#' @examples
#' @seealso
#'  \code{\link[arrow]{open_dataset}}

arrow_read_table <- function(
    table,
    db = getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/"),
    hive_style = TRUE,
    unify_schemas = FALSE,
    format = c("parquet")) {
  dataset <- arrow::open_dataset(
    sources = file.path(db, table),
    hive_style = hive_style,
    unify_schemas = unify_schemas,
    format = format
  )

  return(dataset)
}
