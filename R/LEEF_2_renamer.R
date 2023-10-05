#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname LEEF_2_rename_species
#' @export
LEEF_2_rename_species <- function(
    x) {
  if (!"species" %in% colnames(x)) {
    return(x)
  } else {
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

    for (i in seq_along(species_names)) {
      trn <- x$species == species_names[[i]][1]
      x$species[trn] <- x$species[trn] <- species_names[[i]][2]
    }

    return(x)
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname LEEF_2_rename_species_prob_columns
#' @export
LEEF_2_rename_species_prob_columns <- function(
    object) {
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

  nms <- names(object) |>
    tolower()

  for (i in seq_along(species_prob_columns)) {
    toRename <- nms %in% species_prob_columns[[i]][1]
    if (any(toRename)) {
      nms[toRename] <- species_prob_columns[[i]][2]
    }
  }
  names(object) <- nms

  return(object)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param object PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname LEEF_2_rename_composition
#' @export
LEEF_2_rename_composition <- function(object) {
  object |>
    rename(
      Chlamydomonas_reinhardtii = Chlamydomonas,
      Coleps_sp = Coleps_irchel,
      Colpidium_striatum = Colpidium,
      Cryptomonas_sp = Cryptomonas,
      Didinium_nasutum = Didinium,
      Euplotes_daidaleos = Euplotes,
      Loxocephalus_sp = Loxocephallus,
      Paramecium_bursaria = Paramecium_bursaria,
      Paramecium_caudatum = Paramecium_caudatum,
      Stylonychia_sp = Stylonychia2
    )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname LEEF_2_rename_toc
#' @export

LEEF_2_rename_toc <- function(x) {
  if (!"inj_type" %in% colnames(x)) {
    return(x)
  } else {
    inj_types <- list(
      c("IC", "DIC"),
      c("TC", "DC"),
      c("TOC", "DOC"),
      c("TN", "DN")
    )
    for (i in seq_along(inj_types)) {
      trn <- x$inj_type == inj_types[[i]][1]
      x$inj_type[trn] <- inj_types[[i]][2]
    }

    return(x)
  }
}
