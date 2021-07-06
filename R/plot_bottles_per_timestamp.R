#' Plot diagnostic plot to check bottles per timestamp
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param config_yml the config file containing the sql queries
#' @param config the configuration in \code{list} format. If not specified,
#'   the \code{config_yml} will be used
#'
#' @return \code{ggplot} object of the plot
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot_bottles_per_timestamp <- function(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  config_yml = system.file("tables.yml", package = "LEEF.analysis"),
  config
){
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

  sql <- NULL
  for (i in 1:7) {
    if (DBI::dbExistsTable(con, config[[i]]$FROM)) {
      sql <- paste(
        sql,
        ifelse(
          length(sql) == 0,
          "",
          "UNION ALL"
        ),
        "SELECT",
        paste0("timestamp, bottle, \"", names(config)[[i]], "\" AS measurement"),
        "FROM",
        config[[i]]$FROM,

        sep = "\n"
      )
    }
  }

  data <- read_sql(db, sql = sql)
  data$timestamp <- as.Date(data$timestamp, "%Y%m%d")
  data$bottle[which(data$bottle == "b_100")] <- "b_c_1"
  data$bottle[which(data$bottle == "b_101")] <- "b_c_2"

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$timestamp, y = .data$bottle)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("") +
    ggplot2::facet_wrap(~measurement)
  p
}
