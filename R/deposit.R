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
#'
#' @return
#'
#' @md
#'
#' @import zen4R
#'
#' @export
#'
#' @examples
deposit <- function(
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

  datadir <- list(
    pre_processed = c(
      stage = "pre_processed",
      path = file.path(
        archive_dir,
        "LEEF.archived.data/LEEF/3.archived.data/pre_processed",
        folder
      )
    ),
    extracted = c(
      stage = "extracted",
      path = file.path(
        extracted = archive_dir,
        "LEEF.archived.data/LEEF/3.archived.data/extracted",
        folder
      )
    )
  )

  # Connect to Zenodo -------------------------------------------------------

  zenodo <- ZenodoManager$new(
    url = url,
    token = token,
    logger = "INFO"
  )

  # Create individual records ------

  recs <- lapply(
    datadir,
    function(x){
      myrec <- zen4R::ZenodoRecord$new()

      myrec$setUploadType("dataset")
      # myrec$setPublicationType(chapters$type[i])
      myrec$setTitle(
        paste0(
          "LEEF Experiment ", x["stage"], " data from ", as.Date(timestamp, "%Y%m%d")
        )
      )
      myrec$setDescription(description)
      myrec$addCreator(
        firstname = "Rainer M",
        lastname = "Krug",
        affiliation = "University of Zurich"
      )
      myrec$setLicense("CC-BY-SA-4.0")
      myrec$setAccessRight("open")
      myrec$setVersion("1.0")
      myrec$setLanguage("eng")
      myrec$setKeywords(c("LEEF-UZH"))
      # myrec$addRelatedIdentifier(
      #   relation = chapters$figures[[i]]$relation[n],
      #   identifier = chapters$figures[[i]]$figureDOI[n]
      # )
      # myrec$addRelatedIdentifier("isPartOf", "http://bookDOI") # the reference to the DOI of Volume
      # myrec$setGrants("654359") # eLTER
      # myrec$setGrants("871126") # eLTER-PPP
      # myrec$setGrants("871128") # eLTER-Plus

      return(myrec)
    }
  )

  # Upload records ----------------------------------------------------------

  recs <- lapply(
    recs,
    function(rec){
      zenodo$depositRecord(rec)
    }
  )

  # Add related identifiers -------------------------------------------------



  # Add data files ----------------------------------------------------------

  recs <- lapply(
    recs,
    function(rec){
      zenodo$uploadFile("path/to/your/file", rec$id)
    }
  )

  # Publish deposits

  recs <- lapply(
    recs,
    function(rec){
      zenodo$publishRecord(rec$id)
    }
  )


}
