#' Rename columns and species in the RRD
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param revert if \code{TRUE}, the changes are reverted. Default: \code{FALSE}
#'
#' @return
#' @export
#'
#' @examples
rename_RRD <- function(
    db = getOption("RRDdb", "LEEF.RRD.sqlite"),
    revert = FALSE
){


  # Rename Columns ----------------------------------------------------------

  # TableName = c("FROM", "TO")
  rename_columns <- list(
    bemovi_mag_16__mean_density_per_ml = c("temperature_treatment", "light_treatment"),
    bemovi_mag_16__morph_mvt = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__mean_density_per_ml = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__mean_density_per_ml_cropped = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__mean_density_per_ml_non_cropped = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__morph_mvt = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__morph_mvt_cropped = c("temperature_treatment", "light_treatment"),
    bemovi_mag_25__morph_mvt_non_cropped = c("temperature_treatment", "light_treatment"),
    experimetal_design = c("temperature", "light_treatment"),
    flowcam__algae_density = c("temperature", "light_treatment"),
    flowcam__algae_traits = c("temperature", "light_treatment")
  )




  # Change Values -----------------------------------------------------------
  tables_with_species <- c(
    "bemovi_mag_16__mean_density_per_ml",
    "bemovi_mag_16__morph_mvt",
    "bemovi_mag_25__mean_density_per_ml",
    "bemovi_mag_25__mean_density_per_ml_cropped",
    "bemovi_mag_25__mean_density_per_ml_non_cropped",
    "bemovi_mag_25__morph_mvt",
    "bemovi_mag_25__morph_mvt_cropped",
    "bemovi_mag_25__morph_mvt_non_cropped",
    "composition",
    "experimetal_design",
    "flowcam__algae_density",
    "flowcam__algae_traits",
    "flowcytometer__flowcytometer_density",
    "manualcount__manualcount_density"
  )


  rename_species <- data.frame(
    old_name = c(
      "Chlamydomonas",
      "Cryptomonas",
      "Cosmarium",
      "Desmodesmus",
      "Monoraphidium",
      "Staurastrum1",
      "Staurastrum2",
      "Colpidium",
      "Dexiostoma",
      "Loxocephallus",
      "Tetrahymena",
      "Coleps_irchel",
      "Paramecium_caudatum",
      "Stylonychia1",
      "Stylonychia2",
      "Euplotes",
      "Paramecium",
      "bursaria",
      "Didinium",
      "ChlamydomonasClumps",
      "DesmodesmusClumps",
      "DigestedAlgae",
      "DividingChlamydomonas",
      "Debris",
      "Debris_and_other",
      "Small_cells",
      "Small_unidentified",
      "airbubbles",
      "OtherCiliate",
      "algae",
      "bacteria",
      "HNA",
      "LNA",
      "MNA"
    ),
    new_name = c(
      "Chlamydomonas reinhardtii",
      "Cryptomonas sp.",
      "Cosmarium botrytis",
      "Desmodesmus armatus",
      "Monoraphidium obtusum",
      "Staurastrum gracile",
      "Staurastrum polytrichum",
      "Colpidium striatum",
      "Dexiostoma campylum",
      "Loxocephalus sp.",
      "Tetrahymena thermophila",
      "Coleps sp.",
      "Paramecium caudatum",
      "Stylonychia mytilus",
      "Stylonychia sp.",
      "Euplotes daidaleos",
      "Paramecium bursaria",
      "Didinium nasutum",
      "Chlamydomonas clumps",
      "Desmodesmus clumps",
      "Digested algae",
      "Dividing Chlamydomonas",
      "Debris",
      "Debris and other",
      "Small cells",
      "Small unidentified",
      "Air bubbles",
      "Other ciliates",
      "Algae",
      "Bacteria",
      "HNA",
      "LNA",
      "MNA"
    )
  )

  if (!revert) {
    # Rename section ----------------------------------------------------------

        # Rename Columns ----------------------------------------------------------

    tables <- names(rename_columns)
    sqls <- lapply(
      tables,
      function(tbn){
        paste0("ALTER TABLE ", tbn, " RENAME COLUMN ", rename_columns[tbn][[1]], " TO ", rename_columns[tbn][[1]])
      }

    )

        # Rename Species ----------------------------------------------------------


  ALTER TABLE table_name
 #   RENAME COLUMN current_name TO new_name;


  } else {
    # Revert Section ----------------------------------------------------------

    # Revert Columns ----------------------------------------------------------

    # Revert Species ----------------------------------------------------------

  }

}
