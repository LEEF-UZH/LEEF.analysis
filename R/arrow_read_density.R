#' Read \code{density} table from an arrow database and return as a tibble.
#'
#' This function returns the actual data.
#' @param db fully qualified path to the folder which contains all the arrow directories,
#'  in case of LEEF called \code{parquet}.  Defaul: \code{getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/")}
#' @param from_timestamp \code{integer}. Earliest timestamp to return. Default: \code{20210920}
#' @param to_timestamp \code{integer}. Latest timestamp to return. Default: \code{21000101}
#'
#' @return \code{tibble} containing the data
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom dplyr tbl relocate mutate collect
#'
#' @export
#'
#' @examples
arrow_read_density <- function(
    db = getOption("RRDarrow", "/Volumes/RRD.Reclassification_LEEF-2/parquet/"),
    from_timestamp = 20221106,
    to_timestamp = 20230812) {
    b_16 <- arrow_read_table("bemovi_16_density", db) |>
        dplyr::select(timestamp, bottle, species, density, biomass) |>
        mutate(measurement = "bemovi_16")
    b_25 <- arrow_read_table("bemovi_25_density", db) |>
        dplyr::select(timestamp, bottle, species, density, biomass) |>
        mutate(measurement = "bemovi_25")
    b_25_c <- arrow_read_table("bemovi_25_density_cropped", db) |>
        dplyr::select(timestamp, bottle, species, density, biomass) |>
        mutate(measurement = "bemovi_25_cropped")
    fcam <- arrow_read_table("flowcam_density", db) |>
        dplyr::select(timestamp, bottle, species, density, biomass) |>
        mutate(measurement = "flowcam")
## TODO remove biomass NA when set!
    fcyt <- arrow_read_table("flowcytometer_density", db) |>
        dplyr::select(timestamp, bottle, species, density) |>
        mutate(biomass = as.numeric(NA)) |>
        mutate(measurement = "flowcytometer")
    manualcount <- arrow_read_table("manualcount", db) |>
        dplyr::select(timestamp, bottle, species, density) |>
        mutate(timestamp = as.integer(timestamp)) |>
        mutate(biomass = as.numeric(NA)) |>
        mutate(measurement = "manualcount")
    exp_des <- arrow_read_table("experimental_design", db)

    density <- b_16 |>
        dplyr::union(b_25) |>
        dplyr::union(b_25_c) |>
        dplyr::union(fcam) |>
        dplyr::union(fcyt) |>
        dplyr::union(manualcount) |>
        full_join(exp_des, by = "bottle") |>
        filter(as.integer(timestamp) >= as.integer(from_timestamp)) |>
        filter(as.integer(timestamp) <= as.integer(to_timestamp))

    density <- density |>
        dplyr::collect() |>
        dplyr::mutate(day = as.integer(difftime(as.Date(as.character(timestamp), format = "%Y%m%d"), as.Date("2022-11-07"), units = "days"))) |>
        dplyr::relocate(day, .after = timestamp)

    return(density)
}
