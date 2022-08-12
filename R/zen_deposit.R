#' Create list containing the bibligraphic metadata for the data deposit
#'
#' @param community community to which the deposited data should be added
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
zen_metadata_bib <- function(
    community = "leef-uzh",
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

    data_dir = "./tmp",

    metadata_bib = zen_metadata_bib(),

    publish = FALSE,
    sandbox = TRUE
){

  # For Safety during testing -----------------------------------------------


  message("Publishing is disabled until finalising of the Function!")
  message("Sandbox is always used until finalising of the Function!")
  publish <- FALSE
  sandbox <- TRUE


  # Preparations ------------------------------------------------------------


  if (sandbox) {
    url <- "https://sandbox.zenodo.org/api"
  } else {
    url <- "https://zenodo.org/api"
  }
  ##

  deposits <- list(
    pre_processed = list(
      stage = "pre_processed",
      data = file.path(
        data_dir,
        paste0("data_pre_processed_", timestamp, ".zip")
      ),
      metadata = file.path(
        data_dir,
        paste0("metadata_pre_processed.zip")
      )
    ),
    extracted = list(
      stage = "extracted",
      data = file.path(
        data_dir,
        paste0("data_extracted_", timestamp, ".zip")
      ),
      metadata = file.path(
        data_dir,
        paste0("metadata_extracted.zip")
      )
    )
  )

  lapply(
    deposits,
    function(x){
      if (!file.exists(x$data)) {
        stop("Data file for stage ", x$stage, " does not exist!")
      }
      if (!file.exists(x$metadata)) {
        stop("Metadata file for stage ", x$stage, " does not exist!")
      }
    }
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


      # Create and populate records -------------------------------------------------


      rec <- zen4R::ZenodoRecord$new()

      rec$setUploadType(metadata_bib$upload_type)
      title = paste0("LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d"))
      rec$setTitle(title)
      #   paste0(
      #     "LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d")
      #   )
      # )
      lapply(
        metadata_bib$authors,
        function(aut){
          rec$addCreator(
            firstname = aut$firstname,
            lastname = aut$lastname,
            affiliation = aut$affiliation,
            orcid = aut$orcid
          )
        }
      )
      rec$setDescription(metadata_bib$description)
      rec$setVersion(metadata_bib$version)
      rec$setLanguage(metadata_bib$language)
      keywords <- c(
        metadata_bib$keywords,
        x["stage"],
        paste0("timestamp_", timestamp),
        as.Date(timestamp, "%Y%m%d")
      )
      rec$setKeywords(metadata_bib$keywords)
      rec$setLicense(metadata_bib$license)
      rec$setAccessRight(metadata_bib$access_right)
      # rec$setGrants("654359") # eLTER
      # rec$setGrants("871126") # eLTER-PPP
      # rec$setGrants("871128") # eLTER-Plus
      lapply(
        metadata_bib$contributors,
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


      # Upload records ----------------------------------------------------------


      x$rec <- zenodo$depositRecord(x$rec)
      x$doi <- x$rec$metadata$prereserve_doi$doi


      # Upload data files ----------------------------------------------------------


      zenodo$uploadFile(x$metadata, x$rec)
      zenodo$uploadFile(x$data, x$rec)


      # End ---------------------------------------------------------------------


      return(x)
    }
  )


  # Add related identifiers -------------------------------------------------


  deposits[["pre_processed"]]$rec$addRelatedIdentifier(
    identifier = deposits[["extracted"]]$doi,
    relation = "isDerivedFrom",
    resource_type = "dataset"
  )
  deposits[["pre_processed"]]$rec <- zenodo$depositRecord(deposits[["pre_processed"]]$rec)

  deposits[["extracted"]]$rec$addRelatedIdentifier(
    identifier = deposits[["pre_processed"]]$doi,
    relation = "isSourceOf",
    resource_type = "dataset"
  )
  deposits[["extracted"]]$rec <- zenodo$depositRecord(deposits[["extracted"]]$rec)


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


#' Title
#'
#' @param to_dir directory in which the compressed data folders should be saved to
#' @param archive_dir base directory of the data archive
#' @param timestamp timestamp to be uploaded
#' @param stage stage of the data. Allowed values are \code{"pre_processed"}, \code{"extracted"}
#'
#' @return names of the created data zip archives
#' @export
#'
zen_create_data_archives <- function(
    to_dir = ".",
    archive_dir = "~/Duck/LEEFSwift3",
    timestamp,
    stage = c("pre_processed", "extracted")
){


  # Helper function - comp --------------------------------------------------


  comp <- function(datapath, timestamp, zipfile){
    # zip -9X ~/tmp/data_20220406.zip  *.20220406/*

    olddir <- getwd()
    f <- file.path(tempfile())
    result <- NULL
    on.exit(
      {
        setwd(olddir)
        return(result)
      }
    )

    f <- file.path(tempfile())
    dir.create(f)
    f <- file.path(f, "data.zip")

    setwd(file.path(datapath))

    utils::zip(
      zipfile = zipfile,
      flags = "-9X",
      files = file.path(".", paste0("*.", timestamp), "*")
    )
    result <- zipfile
  }


  # get datapath and timestamps ---------------------------------------------


  datapath  <- file.path(
    archive_dir,
    "LEEF.archived.data/LEEF/3.archived.data",
    stage
  )

  dirs <- list.dirs(datapath, full.names = FALSE, recursive = FALSE)
  timestamps <- gsub("^(.*?)\\.202", "202", dirs)
  timestamps <- gsub("^(.*?)\\.302", "302", timestamps)
  timestamps <- unique(timestamps)

  archives <- lapply(
    timestamps,
    function(timestamp){
      list(
        timestamp = timestamp,
        zipfile = file.path(
          to_dir, paste0(
            "data", "_",
            stage, "_",
            timestamp, ".",
            "zip"
          )
        )
      )
    }
  )


  # Archive all timestamps --------------------------------------------------

  result <- parallel::mclapply(
    archives,
    function(x) {
      comp(
        datapath = datapath,
        timestamp = x$timestamp,
        zipfile = x$zipfile
      )
    },
  )

  return(result)
}
