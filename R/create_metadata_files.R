get_table_pragma <- function(db, table) {
  sql <- paste0(
    '
    SELECT
    	"', table, '" AS table_name,
  	  name AS column_name,
	    type
    FROM
      pragma_table_info("', table, '");
    '
  )
  read_sql(db, sql)
}

get_mimema <- function(db, table, column) {
  sql <- paste0(
    '
    SELECT
      MIN("', column, '") AS min,
      AVG("', column, '") AS mean,
      MAX("', column, '") AS max,
      COUNT(DISTINCT "', column, '") AS no_distict_values
    FROM
      "', table, '";
    '
  )
  mimema <- read_sql(db, sql)
  if (is.character(mimema[[1]])) {
    mimema[[1]] <- as.numeric(NA)
    mimema[[2]] <- as.numeric(NA)
    mimema[[3]] <- as.numeric(NA)
  }

  mimema[[4]] <- as.integer(mimema[[4]])
  names(mimema) <- c("min", "mean", "max", "no_distict_values")
  return(mimema)
}

get_values <- function(db, table, column, max_values = 50) {
  sql <- paste0(
    '
    SELECT DISTINCT
      "', column, '" AS "values"
    FROM
      "', table, '";
    '
  )
  values <- read_sql(db, sql)[["values"]]
  values <- sort(values)
  if (length(values) >= max_values){
    values <- c(
      values[1:(max_values/2)],
      "...",
      values[1:(max_values/2)]
    )
  }
}

get_column_info <- function(
    db,
    table_metadata_row,
    mimema = NULL,
    values = NULL,
    max_values = 50
){
  if (is.null(mimema)){
    mimema <- !(table_metadata_row$type %in% c("TEXT", "REAL"))
  }
  if (is.null(values)){
    values <- table_metadata_row$type %in% c("TEXT", "INTEGER")
  }

  mimema_d <- mimema
  mimema <- get_mimema(
    db,
    table_metadata_row$table_name,
    table_metadata_row$column_name
  )
  if (!mimema_d){
    mimema <- c(
      min = as.numeric(NA),
      mean = as.numeric(NA),
      max = as.numeric(NA),
      no_distict_values = mimema[["no_distict_values"]]
    )
  }

  if (values){
    values <- get_values(
      db,
      table_metadata_row$table_name,
      table_metadata_row$column_name,
      max_values = max_values
    )
  } else {
    values <- as.numeric(NA)
  }

  result <- data.frame(
    min  = mimema[["min"]],
    mean = mimema[["mean"]],
    max  = mimema[["max"]],
    no_distict_values = mimema[["no_distict_values"]],
    values = paste0(values, collapse = ", ")
  )
  return(result)
}

get_table_metadata <- function(
    db,
    table,
    mimema = NULL,
    values = NULL,
    max_values = 50
){

  tmd <- get_table_pragma(db, table)

  cmd <- lapply(
    1:nrow(tmd),
    function(i){
      message(" |-- Processing column ", i, " out of ", nrow(tmd), " (", tmd[i,]$column_name, ") ...")
      get_column_info(
        db = db,
        table_metadata_row = tmd[i,],
        mimema = mimema,
        values = values,
        max_values = max_values
      )
    }
  )
  cmd <- do.call(rbind, cmd)

  metadata <- cbind(
    tmd,
    unit = as.character(NA),
    description = as.character(NA),
    cmd,
    comments = as.character(NA)
  )
  return(metadata)
}

get_db_metadata <- function(
    db,
    mimema = NULL,
    values = NULL,
    max_values = 50
){
  tables <- db_read_table(db, quiet = TRUE)
  tables <- tables[1:3]
  metadata <- lapply(
    1:length(tables),
    function(i){
      message("Processing table ", i, " out of ", length(tables), " (", tables[i], ") ...")
      metadata <- get_table_metadata(
        db = db,
        table = tables[i],
        mimema = mimema,
        values = values,
        max_values = max_values
      )
      write.csv(metadata, paste0("metadata.",tables[i], ".csv"))
      return(metadata)
    }
  )
  names(metadata) <- tables
  invisible(metadata)
}
