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
RRD_LEEF_1_rename <- function(
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
    c("flowcam__algae_traits", "temperature", "light_treatment"),
    c("composition", "Chlamydomonas", "Chlamydomonas_reinhardtii"),
    c("composition", "Cryptomonas", "Cryptomonas_sp"),
    c("composition", "Monoraphidium", "Monoraphidium_obtusum"),
    c("composition", "Cosmarium", "Cosmarium_botrytis"),
    c("composition", "Staurastrum1", "Staurastrum_gracile"),
    c("composition", "Staurastrum2", "Staurastrum_polytrichum"),
    c("composition", "Desmodesmus", "Desmodesmus_armatus"),
    c("composition", "Tetrahymena", "Tetrahymena_thermophila"),
    c("composition", "Colpidium", "Colpidium_striatum"),
    c("composition", "Loxocephallus", "Loxocephalus_sp"),
    c("composition", "Dexiostoma", "Dexiostoma_campylum"),
    c("composition", "Paramecium_caudatum", "Paramecium_caudatum"),
    c("composition", "Stylonychia1", "Stylonychia_mytilus"),
    c("composition", "Stylonychia2", "Stylonychia_sp"),
    c("composition", "Coleps_irchel", "Coleps_sp"),
    c("composition", "Paramecium_bursaria", "Paramecium_bursaria"),
    c("composition", "Euplotes", "Euplotes_daidaleos"),
    c("composition", "Didinium", "Didinium_nasutum")
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
    "bemovi_mag_16__morph_mvt",
    "bemovi_mag_25__mean_density_per_ml",
    "bemovi_mag_25__mean_density_per_ml_cropped",
    "bemovi_mag_25__mean_density_per_ml_non_cropped",
    "bemovi_mag_25__morph_mvt",
    "bemovi_mag_25__morph_mvt_cropped",
    "bemovi_mag_25__morph_mvt_non_cropped",
    "flowcam__algae_density",
    "flowcam__algae_traits",
    "flowcytometer__flowcytometer_density",
    "manualcount__manualcount_density"
  )


  # c("FROM", "TO")
  species_names <- list(
    c("Chlamydomonas", "Chlamydomonas reinhardtii"),
    c("Cryptomonas", "Cryptomonas sp."),
    c("Cosmarium", "Cosmarium botrytis"),
    c("Desmodesmus", "Desmodesmus armatus"),
    c("Monoraphidium", "Monoraphidium obtusum"),
    c("Staurastrum1", "Staurastrum gracile"),
    c("Staurastrum2", "Staurastrum polytrichum"),
    c("Colpidium", "Colpidium striatum"),
    c("ColpidiumVacuoles", "Colpidium vacuoles"),
    c("Dexiostoma", "Dexiostoma campylum"),
    c("Loxocephallus", "Loxocephalus sp."),
    c("Tetrahymena", "Tetrahymena thermophila"),
    c("Coleps_irchel", "Coleps sp."),
    c("Paramecium_caudatum", "Paramecium caudatum"),
    c("Stylonychia1", "Stylonychia mytilus"),
    c("Stylonychia2", "Stylonychia sp."),
    c("Euplotes", "Euplotes daidaleos"),
    c("Paramecium_bursaria", "Paramecium bursaria"),
    c("Didinium", "Didinium nasutum"),
    c("ChlamydomonasClumps", "Chlamydomonas clumps"),
    c("DesmodesmusClumps", "Desmodesmus clumps"),
    c("DigestedAlgae", "Digested algae"),
    c("DividingChlamydomonas", "Dividing Chlamydomonas"),
    c("Debris", "Debris"),
    c("Debris_and_other", "Debris and other"),
    c("Small_cells", "Small cells"),
    c("Small_unidentified", "Small unidentified"),
    c("airbubbles", "Air bubbles"),
    c("OtherCiliate", "Other ciliates"),
    c("algae", "Algae"),
    c("bacteria", "Bacteria"),
    c("HNA", "HNA"),
    c("LNA", "LNA"),
    c("MNA", "MNA")
  )



# Define SQLs -------------------------------------------------------------------


  if (!revert) {


    # Define temp to light column names ---------------------------------------


    sqls_temp_light <- lapply(
      column_names,
      function(td){
        paste0("ALTER TABLE '", td[1], "' RENAME COLUMN '", td[[2]], "' TO '", td[[3]], "'")
      }
    ) |> unlist()

    # Define _prob column names -----------------------------------------------


    sqls_prob <- lapply(
      species_prob_columns,
      function(td){
        paste0("ALTER TABLE '", tables_with_prob_column, "' RENAME COLUMN '", td[[1]], "' TO '", td[[2]], "'")
      }
    ) |>
      unlist()


    # Define species column values --------------------------------------------

    sqls_species_names <- lapply(
      species_names,
      function(td){
        paste0(
          " UPDATE '", tables_with_species_column, "'",
          " SET species = '", td[[2]], "'",
          " WHERE ",
          " species = '" , td[[1]], "'"
        )
      }
    ) |>
      unlist()


    # Combine SQLs  -----------------------------------------------------------


    sqls <- c(sqls_temp_light, sqls_prob, sqls_species_names)



  } else {


    # Define temp to light column names ---------------------------------------


    sqls_temp_light <- lapply(
      column_names,
      function(td){
        paste0("ALTER TABLE '", td[1], "' RENAME COLUMN '", td[[3]], "' TO '", td[[1]], "'")
      }
    ) |> unlist()

    # Define _prob column names -----------------------------------------------


    sqls_prob <- lapply(
      species_prob_columns,
      function(td){
        paste0("ALTER TABLE '", tables_with_prob_column, "' RENAME COLUMN '", td[[2]], "' TO '", td[[1]], "'")
      }
    ) |>
      unlist()


    # Define species column values --------------------------------------------

    sqls_species_names <- lapply(
      species_names,
      function(td){
        paste0(
          " UPDATE '", tables_with_species_column, "'",
          " SET species = '", td[[1]], "'",
          " WHERE ",
          " species = '" , td[[2]], "'"
        )
      }
    ) |>
      unlist()


    # Combine SQLs  -----------------------------------------------------------


    sqls <- c(sqls_temp_light, sqls_prob, sqls_species_names)

  }

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
          DBI::dbExecute(conn, sql)
        }
      )
    }
  )
  cat("Done\n")


  DBI::dbCommit(conn)

}
