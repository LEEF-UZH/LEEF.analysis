#' Deposit data on Zenodo
#'
#' @param token Zenodo token to upload the deposit
#' @param sandbox if `TRUE` (default) upload to the Zenodo sandbox for testing, if `FALSE` upload to the "real" Zenodo
#' @param timestamp timestamp to be uploaded
#' @param measuring_method measuring method to be uploaded. Allowed values at the moment:
#'    - `"bemovi.mag.16"`
#'    - `"bemovi.mag.25"`
#'    - `"flowcam"`
#'    - `"flowcytometer"`
#'    - `"o2meter"`
#'    - `"manualcount"`
#' @param archive_dir root directory of the archive directories
#' @param description Description of the two data deposits
#'
#' @return list with all info concerning the deposits
#'
#' @md
#'
#' @importFrom utils zip
#' @importFrom zen4R ZenodoManager ZenodoRecord
#'
#' @export
#'
zen_deposit <- function(
    token = NULL,
    sandbox = TRUE,
    archive_dir = "~/Duck/LEEFSwift3",
    timestamp,
    measuring_method,
    description = "Description of the data"
){
  if (sandbox) {
    url <- "https://sandbox.zenodo.org/api"
  } else {
    url <- "https://zenodo.org/api"
  }
  ##

  if (any(grepl("bemovi", measuring_method))) {
    folder <- paste0("LEEF.", measuring_method, ".bemovi.", as.character(timestamp))
  } else {
    folder <- paste0("LEEF.fast.", measuring_method, ".", as.character(timestamp))
  }

  deposits <- list(
    pre_processed = list(
      stage = "pre_processed",
      datapath = file.path(
        archive_dir,
        "LEEF.archived.data/LEEF/3.archived.data/pre_processed",
        folder
      )
    ),
    extracted = list(
      stage = "extracted",
      datapath = file.path(
        extracted = archive_dir,
        "LEEF.archived.data/LEEF/3.archived.data/extracted",
        folder
      )
    )
  )

  # Connect to Zenodo -------------------------------------------------------

  zenodo <- zen4R::ZenodoManager$new(
    url = url,
    token = token,
    logger = "INFO"
  )

  # Create individual records ------

  deposits <- lapply(
    deposits,
    function(x){
      rec <- zen4R::ZenodoRecord$new()

      rec$setUploadType("dataset")
      # rec$setPublicationType(chapters$type[i])
      rec$setTitle(
        paste0(
          "LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d")
        )
      )
      rec$setDescription(description)
      rec$addCreator(
        firstname = "Rainer M",
        lastname = "Krug",
        affiliation = "University of Zurich"
      )
      rec$setLicense("CC-BY-SA-4.0")
      rec$setAccessRight("open")
      rec$setVersion("1.0")
      rec$setLanguage("eng")
      rec$setKeywords(c("LEEF-UZH"))
      # rec$addRelatedIdentifier(
      #   relation = chapters$figures[[i]]$relation[n],
      #   identifier = chapters$figures[[i]]$figureDOI[n]
      # )
      # rec$addRelatedIdentifier("isPartOf", "http://bookDOI") # the reference to the DOI of Volume
      # rec$setGrants("654359") # eLTER
      # rec$setGrants("871126") # eLTER-PPP
      # rec$setGrants("871128") # eLTER-Plus

      x$rec <-  rec

      return(x)
    }
  )


  # Upload records ----------------------------------------------------------

  deposits <- lapply(
    deposits,
    function(x){
      x$rec <- zenodo$depositRecord(x$rec)
      x$doi <- x$rec$metadata$prereserve_doi$doi
      return(x)
    }
  )

  # Add related identifiers -------------------------------------------------


  deposits[["pre_processed"]]$rec$addRelatedIdentifier(
      relation = "isCompiledBy",
      identifier = deposits[["extracted"]]$doi
    )
  deposits[["pre_processed"]]$rec <- zenodo$depositRecord(deposits[["pre_processed"]]$rec)

  deposits[["extracted"]]$rec$addRelatedIdentifier(
    relation = "compiles",
    identifier = deposits[["pre_processed"]]$doi
  )
  deposits[["extracted"]]$rec <- zenodo$depositRecord(deposits[["extracted"]]$rec)

  # Add data files ----------------------------------------------------------

  recs <- lapply(
    deposits,
    function(x){
      olddir <- getwd()
      f <- file.path(tempfile())
      on.exit(
        {
          setwd(olddir)
          if (dir.exists(dirname(f))) {
            unlink(dirname(f), recursive = TRUE)
          }
        }
      )
      f <- file.path(tempfile())
      dir.create(f)
      f <- file.path(f, "data.zip")

      olddir <- setwd(file.path(x$datapath, ".."))
      utils::zip(
        zipfile = f,
        files =  basename(x$datapath)
      )
      zenodo$uploadFile(f, x$rec)
    }
  )

  # Publish deposits

  recs <- lapply(
    deposits,
    function(x){
      zenodo$publishRecord(x$rec$id)
    }
  )

  return(deposits)
}
