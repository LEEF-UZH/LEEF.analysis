#' Title
#'
#' @param db fully qualified path to the sqlite database.Must be set!
#' @param csv_file name of the csv files to be imported
#' @param measure measure
#' @param tn_postfix postfix for the table name,
#' @param archive_dir directory containes the archive. It contains the following directories:
#' - LEEF.archived.data
#' - LEEF.archived.data_segments
#' - LEEF.backend.data
#' - LEEF.backend.data_segments
#'
#' @return
#'
#' @importFrom  pbapply  pbsapply
#' @export
#'
#' @examples
add_extracted_csv_to_db <- function(
    db,
    csv_file = "Morph_mvt_non_cropped.csv",
    measure = "bemovi.mag.25",
    tn_postfix = "non_cropped",
    archive_dir = "/Users/rainerkrug/Duck/LEEFSwift3",
    copy_locally_first = FALSE
){

  if (missing(db)) {
    stop("db must be specified!")
  }



# Determining file names to be added --------------------------------------



  message("\nCollecting csv files - this might take some time!\n")

  dirs <- list.files(
    path = file.path(archive_dir, "LEEF.archived.data","LEEF", "3.archived.data", "extracted"),
    pattern = measure,
    recursive = FALSE,
    full.names = TRUE
  )

  input_files <- sapply(
    dirs,
    function(d)
    list.files(
      path = d,
      pattern = csv_file,
      recursive = FALSE,
      full.names = TRUE
    )
  ) |> unlist()

  table_name <-  LEEF.backend.sqlite::tn_from_fn(basename(csv_file), measure = measure, tn_postfix = tn_postfix)


# Add to database ---------------------------------------------------------


  message("\nAdding csv files to database - this will take much longer!\n")


  conn <- NULL
  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db = db
  )
  on.exit({
    try(DBI::dbDisconnect(conn), silent = TRUE)
  })

  result <- pbapply::pbsapply(
    input_files,
    function(fn) {
      message("\nProcessing ", fn, "...")
      message("     |-Reading file ...")
      if (copy_locally_first){
        temp_fn <- tempfile(fileext = ".csv")
        file.copy(fn, temp_fn)
      } else {
        temp_fn <- fn
      }
      dat <- read.csv(temp_fn)
      message("     |-Adding data ...")
      if (grepl("flowcam", table_name)){
        if (!("filtration" %in% names(dat))) {
          dat$filtration <- as.numeric(NA)
        }
        if (!("flowcell" %in% names(dat))) {
          dat$flowcell <- as.character(NA)
        }
        if (!("instrument" %in% names(dat))) {
          dat$instrument <- as.character(NA)
        }
        if (!("instrument" %in% names(dat))) {
          dat$instrument <- as.character(NA)
        }
        if (!("DividingChlamydomonas_prob" %in% names(dat))) {
          dat$DividingChlamydomonas_prob <- as.numeric(NA)
        }
        if (!("Small_unidentified_prob" %in% names(dat))) {
          dat$Small_unidentified_prob <- as.numeric(NA)
        }
      }
      DBI::dbBegin(conn)
      DBI::dbWriteTable(
        conn,
        name = table_name,
        value = dat,
        overwrite = FALSE,
        append = TRUE
      )
      DBI::dbCommit(conn)
      message("     |-Done")
    }
  )

  names(result) <- input_files
  return(result)
}
