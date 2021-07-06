#' Read data from the RRD database
#'
#' The function uses a config file in yaml format to either build an sql query
#' or the sql query provided to execute the query and return the data in
#' data.frame format. In the config file, either an \code{SQL} statement or a
#' \code{FROM} and \code{SELSECT} fields can be specified. If an \code{SQL}
#' statement is provided, it takes precedence over the others. If no \code{SQL}
#' is specified, the SQL statement is build from the \code{SELECT} and
#' \code{FROM} parts. For security, the database is opened in \bold{read-only}
#' mode.!
#' @param db fully qualified path to the sqlite database
#' @param config_yml the config file containing the sql queries
#' @param config the configuration in \code{list} format. If not specified,
#'   the \code{config_yml} will be used
#' @param measurements the configuration as int the config file specified to be
#'   used
#'
#' @return if \code{length(measurements) > 1} a list containing the returned tables, otherwise the table as a
#'   \code{data.frame} object.
#'
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @importFrom RSQLite SQLite SQLITE_RO
#' @importFrom yaml read_yaml
#' @importFrom parallel mclapply
#' @export
#'
read_rrd <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  measurements = c(
    "bemovi_mag_16",
    "bemovi_mag_25",
    "bemovi_mag_25_cropped",
    "flowcam",
    "flowcytometer",
    "manualcount",
    "o2meter"
  ),
  config_yml = system.file("tables.yml", package = "LEEF.analysis"),
  config
) {
  if (missing(config)) {
    config <- yaml::read_yaml(config_yml)
  }

  con <- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), db, flags = RSQLite::SQLITE_RO)
  on.exit({
    if (class(con) == "RSQLite") {
      DBI::dbDisconnect(con)
    }
  })

  tables <- parallel::mclapply(
    measurements,
    function(m) {
      if (is.null(config[[m]]$SQL)) {
        sql <- paste0("SELECT ", config[[m]]$SELECT, " FROM ", config[[m]]$FROM)
      } else {
        sql <- config[[m]]$SQL
      }
      x <- NULL
      if (DBI::dbExistsTable(con, )) {
        x <- DBI::dbGetQuery(con, sql)
      }
      return(x)
    },
    mc.cores = getOption("mc.cores", 1)
  )

  names(tables) <- measurements
  ##
  if (length(tables) == 1) {
    return(tables[[1]])
  } else {
    return(tables)
  }
}
