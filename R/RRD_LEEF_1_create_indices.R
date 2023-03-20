#' Rename columns and species in the RRD
#'
#' @param db fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
#'   If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}
#' @param revert if \code{TRUE}, the changes are reverted. Default: \code{FALSE}
#'
#' @return
#' @importFrom DBI dbConnect dbBegin dbCommit dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
RRD_LEEF_1_create_indices <- function(
    db = getOption("RRDdb", "LEEF.RRD.sqlite"),
    revert = FALSE
){

  # temp to light column names ----------------------------------------------

  # c("TABLE_NAME", "FROM", "TO")
  column_names <- list(
     c("bemovi_mag_16__mean_density_per_ml", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_16__morph_mvt", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__mean_density_per_ml", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__mean_density_per_ml_cropped", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__mean_density_per_ml_non_cropped", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__morph_mvt", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__morph_mvt_cropped", "temperature_treatment", "light_treatment"),
     c("bemovi_mag_25__morph_mvt_non_cropped", "temperature_treatment", "light_treatment"),
     c("experimetal_design", "temperature", "light_treatment"),
     c("flowcam__algae_density", "temperature", "light_treatment"),
     c("flowcam__algae_traits", "temperature", "light_treatment")
  )


  # _prob column names ------------------------------------------------------


  tables_with_prob_column <- c(
    "bemovi_mag_16__morph_mvt",
    "bemovi_mag_25__morph_mvt",
    "bemovi_mag_25__morph_mvt_cropped",
    "bemovi_mag_25__morph_mvt_non_cropped",
    "flowcam__algae_traits"
  )


  # c("FROM", "TO")
  species_prob_columns <- list(
    c("airbubbles_prob", "Air_bubbles_prob"),
    c("chlamydomonas_prob", "Chlamydomonas_reinhardtii_prob"),
    c("chlamydomonasclumps_prob", "Chlamydomonas_clumps_prob"),
    c("coleps_irchel_prob", "Coleps_sp_prob"),
    c("colpidium_prob", "Colpidium_striatum_prob"),
    c("colpidiumvacuoles_prob", "Colpidium_vacuoles_prob"),
    c("cosmarium_prob", "Cosmarium_botrytis_prob"),
    c("cryptomonas_prob", "Cryptomonas_sp_prob"),
    c("debris_and_other_prob", "Debris_and_other_prob"),
    c("debris_prob", "Debris_prob"),
    c("desmodesmus_prob", "Desmodesmus_armatus_prob"),
    c("desmodesmusclumps_prob", "Desmodesmus_clumps_prob"),
    c("dexiostoma_prob", "Dexiostoma_campylum_prob"),
    c("didinium_prob", "Didinium_nasutum_prob"),
    c("digestedalgae_prob", "Digested_algae_prob"),
    c("dividingchlamydomonas_prob", "Dividing_Chlamydomonas_prob"),
    c("euplotes_prob", "Euplotes_daidaleos_prob"),
    c("loxocephallus_prob", "Loxocephalus_sp_prob"),
    c("monoraphidium_prob", "Monoraphidium_obtusum_prob"),
    c("otherciliate_prob", "Other_ciliates_prob"),
    c("paramecium_bursaria_prob", "Paramecium_bursaria_prob"),
    c("paramecium_caudatum_prob", "Paramecium_caudatum_prob"),
    c("small_cells_prob", "Small_cells_prob"),
    c("small_unidentified_prob", "Small_unidentified_prob"),
    c("staurastrum1_prob", "Staurastrum_gracile_prob"),
    c("staurastrum2_prob", "Staurastrum_polytrichum_prob"),
    c("stylonychia1_prob", "Stylonychia_mytilus_prob"),
    c("stylonychia2_prob", "Stylonychia_sp_prob"),
    c("tetrahymena_prob", "Tetrahymena_thermophila_prob")
  )


  # species column values ---------------------------------------------------


  tables_with_species_column <- c(
    "bemovi_mag_16__mean_density_per_ml",
    "bemovi_mag_25__mean_density_per_ml",
    "bemovi_mag_25__mean_density_per_ml_cropped",
    "bemovi_mag_25__mean_density_per_ml_non_cropped",
    "bemovi_mag_16__morph_mvt",
    "bemovi_mag_25__morph_mvt",
    "bemovi_mag_25__morph_mvt_cropped",
    "bemovi_mag_25__morph_mvt_non_cropped",
    "flowcam__algae_density",
    "flowcam__algae_traits",
    "flowcytometer__flowcytometer_density",
    "manualcount__manualcount_density"
  )

  sqls_species <- lapply(
    tables_with_species_column,
    function(tn){
      paste0(
        "CREATE INDEX ", paste0("idx_", tn, "_species"), " ",
        "on ", tn, "(species)"
      )
    }
  ) |>
    unlist()

  sqls_bottle <- lapply(
    tables_with_species_column,
    function(tn){
      paste0(
        "CREATE INDEX ", paste0("idx_", tn, "_bottle"), " ",
        "on ", tn, "(bottle)"
      )
    }
  ) |>
    unlist()

  sqls_timestamp <- lapply(
    tables_with_species_column,
    function(tn){
      paste0(
        "CREATE INDEX ", paste0("idx_", tn, "_timestamp"), " ",
        "on ", tn, "(timestamp)"
      )
    }
  ) |>
    unlist()


  sqls <- c(sqls_species, sqls_bottle, sqls_timestamp)



  # Run the SQLs ------------------------------------------------------------


  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    db = db
  )
  on.exit({
    try(DBI::dbDisconnect(conn), silent = TRUE)
  })

  DBI::dbBegin(conn)

  lapply(
    sqls,
    function(sql){
      try(
        {
          cat(sql, "...\n")
          DBI::dbGetQuery(conn, sql)
        }
      )
    }
  )
  cat("Done\n")


  DBI::dbCommit(conn)

}
