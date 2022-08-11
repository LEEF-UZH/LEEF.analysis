#' Create list containing the zenodo metadata for the data deposit
#'
#' @param community community to which the dateposited data should be added
#' @param upload_type type of the deposited data
#' @param authors authors of the deposited data. Each contributor
#'   is also a list with the following fields:
#'   - firstname
#'   - lastname
#'   - affiliation
#'   - orcid
#' @param description description of the deposited data
#' @param version version of the deposited data
#' @param language lanuage of the deposited data
#' @param keywords keywords
#' @param access_right access right of the deposited data
#' @param license license of the deposited data
#' @param contributors list of contributors of the deposited data. Each contributor
#'   is also a list with the following fields:
#'   - firstname
#'   - lastname
#'   - type
#'   - affiliation
#'   - orcid
#'
#' @return list containing the metadata necessary for the data deposit
#'
#' @md
#'
#' @export
#'
zen_metadata <- function(
    community = "LEEF Experiment data",
    upload_type = "dataset",
    authors = list(
      RMK = list(
        firstname = "Rainer M",
        lastname = "Krug",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        firstname = "Owen L",
        lastname = "Petchey",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    ),
    description = "Description of the data",
    version = "1.0.0",
    language = "eng",
    keywords = c("LEEZ-UZH", "LEEF-1"),
    access_right = "open",
    license = "CC-BY-SA-4.0",
    contributors = list(
      RMK = list(
        firstname = "Rainer M",
        lastname = "Krug",
        type = "DataManager",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        firstname = "Owen L",
        lastname = "Petchey",
        type = "ProjectLeader",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    )
){
  metadata <- list(
    community = community,
    upload_type = upload_type,
    authors = authors,
    description = description,
    version = version,
    language = language,
    keywords = keywords,
    access_right = access_right,
    license = license,
    contributors = contributors
  )
  return(metadata)
}

#' Deposit data on Zenodo
#'
#' @param token Zenodo token to upload the deposit
#' @param timestamp timestamp to be uploaded
#' @param measuring_method measuring method to be uploaded. Allowed values at the moment:
#'    - `"bemovi.mag.16"`
#'    - `"bemovi.mag.25"`
#'    - `"flowcam"`
#'    - `"flowcytometer"`
#'    - `"o2meter"`
#'    - `"manualcount"`
#' @param metadata metadata as genersted by the function \code{zen_metadata()}
#' @param archive_dir base directory of the data archive
#' @param publish if \code{TRUE} publish the packages immediately - should normally be \code{FALSE}
#' @param sandbox if `TRUE` (default) upload to the Zenodo sandbox for testing, if `FALSE` upload to the "real" Zenodo
#'
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

    timestamp,
    measuring_method,

    archive_dir = "~/Duck/LEEFSwift3",

    metadata = zen_metadata(),

    publish = FALSE,
    sandbox = TRUE
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
        archive_dir,
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

      rec$setUploadType(metadata$upload_type)
      title = paste0("LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d"))
      rec$setTitle(title)
      #   paste0(
      #     "LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d")
      #   )
      # )
      lapply(
        metadata$authors,
        function(aut){
          rec$addCreator(
            firstname = aut$firstname,
            lastname = aut$lastname,
            affiliation = aut$affiliation,
            orcid = aut$orcid
          )
        }
      )
      rec$setDescription(metadata$description)
      rec$setVersion(metadata$version)
      rec$setLanguage(metadata$language)
      keywords <- c(
        metadata$keywords,
        x["stage"],
        measuring_method,
        paste0("timestamp_", timestamp)
      )
      rec$setKeywords(metadata$keywords)
      rec$setLicense(metadata$license)
      rec$setAccessRight(metadata$access_right)
      # rec$setGrants("654359") # eLTER
      # rec$setGrants("871126") # eLTER-PPP
      # rec$setGrants("871128") # eLTER-Plus
      lapply(
        metadata$contributors,
        function(con){
          rec$addContributor(
            firstname = con$firstname,
            lastname = con$lastname,
            type = con$type,
            affiliation = con$affiliation,
            orcid = con$orcid
          )
        }
      )

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

  if (publish) {
    recs <- lapply(
      deposits,
      function(x){
        zenodo$publishRecord(x$rec$id)
      }
    )
  }

  return(deposits)
}

#' Get all measurement_method / timestamp combinations in the archive
#'
#' @param archive_dir base directory of the data archive
#' @param stage either \code{"pre_processed"} or \code{extracted}
#'
#' @return
#' @export
#'
#' @examples
all_archives <- function(
    archive_dir = "~/Duck/LEEFSwift3",
    stage
){
  datapath  <- file.path(
    archive_dir,
    "LEEF.archived.data/LEEF/3.archived.data",
    stage
  )
  dirs <- list.dirs(datapath, full.names = FALSE, recursive = FALSE)
  dirs <- gsub("^LEEF\\.", "", dirs)
  dirs <- gsub("^fast\\.", "", dirs)
  dirs <- gsub("\\.bemovi\\.", "\\.", dirs)
  dirs <- gsub("^bemovi\\.mag\\.", "bemovi_mag_", dirs)
  dirs <- strsplit(dirs, "\\.") |>
    simplify2array() |>
    t() |>
    as.data.frame()
  names(dirs) <- c("measurement_method", "timestamp")
  dirs$measurement_method <- gsub("bemovi_mag_", "bemovi\\.mag\\.", dirs$measurement_method)
}
