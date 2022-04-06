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
      MAX("', column, '") AS max
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

  names(mimema) <- c("min", "mean", "max")
  return(mimema)
}

get_no_distinct_values <- function(db, table, column) {
  sql <- paste0(
    '
    SELECT
      COUNT(DISTINCT "', column, '") AS no_distict_values
    FROM
      "', table, '";
    '
  )
  no_distinct_values <- read_sql(db, sql)
  no_distinct_values[[1]] <- as.integer(no_distinct_values[[1]])
  names(no_distinct_values) <- c("no_distict_values")
  return(no_distinct_values)
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
  if (length(values) >= max_values) {
    values <- c(
      values[1:(max_values / 2)],
      "...",
      values[1:(max_values / 2)]
    )
  }
}

get_column_info <- function(db,
                            table,
                            column,
                            mimema = NULL,
                            values = NULL,
                            max_values = 50) {
  tp <- get_table_pragma(db, table)
  table_metadata_row <- tp[tp["column_name"] == column, ]

  if (is.null(mimema)) {
    mimema <- !(table_metadata_row$type %in% c("TEXT"))
  }
  if (is.null(values)) {
    values <- table_metadata_row$type %in% c("TEXT", "INTEGER")
  }

  mimema_d <- mimema
  if (mimema_d) {
    mimema <- get_mimema(
      db,
      table,
      column
    )
  }
  if (!mimema_d) {
    mimema <- c(
      min = NA,
      mean = NA,
      max = NA
    )
  }

  if (values) {
    values <- get_values(
      db,
      table,
      column,
      max_values = max_values
    )
  } else {
    values <- NA
  }

  no_distict_values <- get_no_distinct_values(
    db,
    table,
    column
  )

  result <- data.frame(
    min = mimema[["min"]],
    mean = mimema[["mean"]],
    max = mimema[["max"]],
    no_distict_values = no_distict_values[["no_distict_values"]],
    values = paste0(values, collapse = ", ")
  )
  return(result)
}

get_table_metadata <- function(db,
                               table,
                               mimema = NULL,
                               values = NULL,
                               max_values = 50) {
  tmd <- get_table_pragma(db, table)

  cmd <- lapply(
    seq(nrow(tmd)),
    function(i) {
      message(" |-- Processing column ", i, " out of ", nrow(tmd), " (", tmd[i, ]$column_name, ") ...")
      get_column_info(
        db = db,
        table = table,
        column = tmd[i, ]$column_name,
        mimema = mimema,
        values = values,
        max_values = max_values
      )
    }
  )
  cmd <- do.call(rbind, cmd)

  metadata <- cbind(
    tmd,
    cmd
  )
  return(metadata)
}

make_additional_table_metadata <- function(db,
                                           table) {
  metadata <- get_table_pragma(db, table)

  metadata <- data.frame(
    table_name = metadata$table_name,
    column_name = metadata$column_name,
    unit = as.character(NA),
    description = as.character(NA),
    comment = as.character(NA)
  )
  return(metadata)
}

get_db_metadata <- function(db,
                            mimema = NULL,
                            values = NULL,
                            max_values = 50,
                            save_table = TRUE,
                            save_db = TRUE,
                            overwrite = TRUE) {
  tables <- db_read_table(db, quiet = TRUE)
  metadata <- lapply(
    seq_len(length(tables)),
    function(i) {
      message("Processing table ", i, " out of ", length(tables), " (", tables[i], ") ...")
      metadata <- get_table_metadata(
        db = db,
        table = tables[i],
        mimema = mimema,
        values = values,
        max_values = max_values
      )
      if (save_table) {
        fn <- paste0("metadata.", basename(db), ".", tables[i], ".csv")
        if (file.exists(fn) & (!overwrite)) {
          warning("File '", fn, "' exists and will not be overwritten!")
        } else {
          write.csv(metadata, fn)
        }
      }
      return(metadata)
    }
  )
  names(metadata) <- tables
  metadata <- do.call(rbind, metadata)
  if (save_db) {
    fn <- paste0("metadata.", basename(db), ".csv")
    if (file.exists(fn) & (!overwrite)) {
      warning("File '", fn, "' exists and will not be overwritten!")
    } else {
      write.csv(metadata, fn)
    }
  }

  invisible(metadata)
}

make_additional_db_metadata <- function(db,
                                        save_table = TRUE,
                                        save_db = TRUE,
                                        overwrite = FALSE) {
  tables <- db_read_table(db, quiet = TRUE)
  metadata <- lapply(
    seq_len(length(tables)),
    function(i) {
      message("Processing table ", i, " out of ", length(tables), " (", tables[i], ") ...")
      metadata <- make_additional_table_metadata(
        db = db,
        table = tables[i]
      )
      if (save_table) {
        fn <- paste0("metadata.additional.", basename(db), ".", tables[i], ".csv")
        if (file.exists(fn) & (!overwrite)) {
          warning("File '", fn, "' exists and will not be overwritten!")
        } else {
          write.csv(metadata, fn)
        }
      }
      return(metadata)
    }
  )
  names(metadata) <- tables
  metadata <- do.call(rbind, metadata)
  if (save_db) {
    fn <- paste0("metadata.additional.", basename(db), ".csv")
    if (file.exists(fn) & (!overwrite)) {
      warning("File '", fn, "' exists and will not be overwritten!")
    } else {
      write.csv(metadata, fn)
    }
  }

  invisible(metadata)
}

create_final_db_metadata <- function(db,
                                     mimema = NULL,
                                     values = NULL,
                                     max_values = 50,
                                     save_table = TRUE,
                                     save_db = TRUE,
                                     overwrite = TRUE) {
  fn <- paste0("metadata.", basename(db), ".csv")
  if (file.exists(fn)) {
    metadata <- read.csv(fn)
  } else {
    metadata <- get_db_metadata(
      db = db,
      mimema = mimema,
      values = values,
      max_values = max_values,
      save_table = FALSE,
      save_db = FALSE,
      overwrite = FALSE
    )
  }

  fn <- paste0("metadata.additional.", basename(db), ".rds")
  if (file.exists(fn)) {
    additional_metadata <- readRDS(fn)
  } else {
    additional_metadata <- make_additional_db_metadata(
      db = db,
      save_table = FALSE,
      save_db = FALSE,
      overwrite = FALSE
    )
  }

  all_metadata <- merge(
    x = do.call(rbind, metadata),
    y = do.call(rbind, additional_metadata),
    by = c("table_name", "column_name")
  )

  if (save_db) {
    fn <- paste0("metadata.additional.", basename(db), ".csv")
    if (file.exists(fn) & (!overwrite)) {
      warning("File '", fn, "' exists and will not be overwritten!")
    } else {
      write.csv(metadata, fn)
    }
  }

  all_metadata <- split(all_metadata, all_metadata$table_name)

  for (i in seq_len(length(all_metadata))) {
    if (save_table) {
      fn <- paste0("metadata.all.", basename(db), ".", all_metadata[i], ".csv")
      if (file.exists(fn) & (!overwrite)) {
        warning("File '", fn, "' exists and will not be overwritten!")
      } else {
        write.csv(all_metadata[[i]], fn)
      }
    }
  }
}