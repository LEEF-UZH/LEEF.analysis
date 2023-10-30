#' Read \code{toc} table from an arrow database and return as a tibble.
#'
#' This function returns the actual data.
#' @param db fully qualified path to the folder which contains all the arrow directories,
#'  in case of LEEF called \code{parquet}. Defaul: \code{getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/")}
#' @param from_timestamp \code{integer}. Earliest timestamp to return. Default: \code{20210920}
#' @param to_timestamp \code{integer}. Latest timestamp to return. Default: \code{21000101}
#'
#' @return \code{tibble} containing the data
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl mutate relocate collect
#'
#' @export
#'
#' @examples
arrow_read_toc <- function(
# TODO: ADD MEAN CALCULATIONS AS IN DB_READ_TOC()
 
    db = getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/"),
    from_timestamp = 20221106,
    to_timestamp = 20230812) {

    exp_des <- arrow_read_table("experimental_design", db)

    toc <- arrow_read_table("toc", db) |>
        dplyr::select(timestamp, bottle, type = inj_type, concentration = conc, cv) |>
        filter(bottle != "") |>
        mutate(timestamp = as.integer(timestamp)) |>
        filter(as.integer(timestamp) >= as.integer(from_timestamp)) |>
        filter(as.integer(timestamp) <= as.integer(to_timestamp)) |>
        full_join(exp_des, by = "bottle")


    toc <- toc |>
        dplyr::collect() |>
        dplyr::mutate(day = as.integer(difftime(as.Date(as.character(timestamp), format = "%Y%m%d"), as.Date("2022-11-07"), units = "days"))) |>
        dplyr::relocate(day, .after = timestamp) |>
        tibble::as_tibble()

    return(toc)
}
