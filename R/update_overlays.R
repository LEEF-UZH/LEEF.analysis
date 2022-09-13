#' Just a function to update the overlays.
#'
#' @param extracted_dir directory with extracted data to be used
#' @param avi_dir directory in which all avis are in the subdirectories.
#'
#' @return
#' @export
#'
update_overlays <- function(
    extracted_dir = "/Volumes/LEEF/LEEF.archived.data/LEEF/3.archived.data/extracted/",
    avi_dir = "~/Duck/LEEFSwift3/LEEF.archived.data/LEEF/3.archived.data/pre_processed"
){
  bemovi_dirs <- function(
  ){
    bemovis <- list.files(
      extracted_dir,
      pattern = "^LEEF\\.bemovi\\.mag\\..*\\.bemovi\\..*$",
    )
    return(bemovis)
  }

  bemovi_ymls <- function(
    bemovi_dir
  ){
    ymls <- list.files(
      path = file.path(extracted_dir, bemovi_dir),
      pattern = "^bemovi_extract\\..*\\.*\\.yml$"
    )
    return(ymls)
  }



  pbmcapply::pbmclapply(
    bemovi_dirs(),
    function(bemovi_dir){
      bemovi_configs <- bemovi_ymls(bemovi_dir)
      lapply(
        bemovi_configs,
        function(bemovi_config, bemovi_dir){
          bc <- yaml::read_yaml(file.path(extracted_dir, bemovi_dir, bemovi_config))

          temp_overlay_folder <- file.path( extracted_dir, bemovi_dir, bc$temp.overlay.folder)
          overlay_folder <- file.path( extracted_dir, bemovi_dir, bc$overlay.folder)

          unlink(overlay_folder, recursive = TRUE, force = TRUE)
          dir.create(overlay_folder, recursive = TRUE)

          unlink(temp_overlay_folder, recursive = TRUE, force = TRUE)
          dir.create(temp_overlay_folder, recursive = TRUE)

          LEEF.analysis::overlays_from_folders(
            traj_data_file = file.path( extracted_dir, bemovi_dir, bc$merged.data.folder, bc$master ),
            avi_dir = file.path(avi_dir, bemovi_dir),
            bemovi_extract_yml_file = file.path(extracted_dir, bemovi_dir, bemovi_config),
            temp_overlay_folder = temp_overlay_folder,
            overlay_folder = overlay_folder,
            overlay_type = "both",
            label = "species",
            ffmpeg = "ffmpeg",
            font_size = 24,
            circle_size = 120,
            crf = 23,
            gamma = 2,
            mc_cores = cores
          )
        },
        bemovi_dir = bemovi_dir
      )
    }
  )

}
