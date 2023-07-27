#' @export
LEEF_2_rename_composition <- function(object){
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
