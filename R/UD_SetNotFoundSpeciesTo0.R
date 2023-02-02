#' Set number of Not Found species to 0
#'
#' ADD DESCRIPTION
#'
#' @param densities
#' @param mb
#' @param meas
#' @param exp_design
#' @param compositions
#' @param species.tracked
#'
#' @importFrom dplyr filter distinct full_join
#'
#' @return
#'
#' @export
#'
#' @examples
SetNotFoundSpeciesTo0 <- function(
    densities,
    mb,
    meas,
    exp_design,
    compositions,
    species.tracked
    ){
  # create df that has all species tracked that could be there
  if(meas == "bemovi_mag_25_non_cropped") {
    mb <- mb %>% dplyr::filter(timestamp >= 20211103)
  }

  temp <- apply(mb, 1, function(row){
    timestamp <- row["timestamp"]
    bot <- row["bottle"]
    comp <- unlist(exp_design[exp_design$bottle==bot,"composition"])
    specs <- unlist(compositions[compositions$composition==comp,])
    specs <- names(which(specs==1))
    specs <- specs[specs %in% species.tracked]
    if(meas %in% c("bemovi_mag_25_cropped","bemovi_mag_25_non_cropped")) {
      specs <- c(specs,"Cryptomonas","Debris_and_other")
    }
    df <- data.frame(timestamp=timestamp,
                     bottle=bot,
                     composition=comp,
                     species=specs,
                     measurement=meas,
                     row.names = NULL)
  })
  temp <- do.call("rbind", temp) %>% dplyr::distinct()
  # set species that could be present but arent to 0
  densities <- dplyr::full_join(densities,temp) #any NAs?
  densities$density <- ifelse(is.na(densities$density),0,densities$density)
  return(densities)
}
