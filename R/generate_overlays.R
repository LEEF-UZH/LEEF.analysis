#' Just a function to update the overlays.
#'
#' @param params list of parameter
#' @param overwrite if `TRUE`, existing overlays will be ov erwritten. Default is `FALSE`
#'
#' @return
#'
#' @importFrom pbmcapply  pbmclapply
#' @export
#'
generate_overlays <- function(
    params = list(
      cores = 7,
      pre_processed_folder = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/pre_processed/",
      extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
      output_dir = "./overlays/"
    ),
    overwrite = FALSE
){
  bemovi_dirs <- function(
  ){
    bemovis <- list.files(
      params$extracted_dir,
      pattern = "^LEEF\\.bemovi\\.mag\\..*\\.bemovi\\..*$",
    )
    return(bemovis)
  }

  bemovi_ymls <- function(
    bemovi_dirs
  ){
    lapply(
      bemovi_dirs,
      function(bemovi_dir){
        ymls <- list.files(
          path = file.path(params$extracted_dir, bemovi_dir),
          pattern = "^bemovi_extract\\..*\\.*\\.yml$"
        )
        list(
          bemovi_dir = bemovi_dir,
          ymls = ymls
        )
      }
    )
  }

  message("Generating list off ymls to be used...")
  ymls <- bemovi_ymls(bemovi_dirs = bemovi_dirs())

  # saveRDS(ymls, "ymls.rds")

  message("Generating overlays for ", length(ymls), " bemovi sessions. This will take some time...")

  result <- lapply(
    ymls,
    function(yml){
      bemovi_configs <- yml$ymls
      lapply(
        bemovi_configs,
        function(bemovi_config, bemovi_dir){
          bc <- yaml::read_yaml(file.path(params$extracted_dir, bemovi_dir, bemovi_config))

          temp_overlay_folder <- file.path( params$output_dir, bemovi_dir, bc$temp.overlay.folder)
          overlay_folder <- file.path( params$output_dir, bemovi_dir, bc$overlay.folder)

          dir.create(temp_overlay_folder, recursive = TRUE, showWarnings = FALSE)
          dir.create(overlay_folder, recursive = TRUE, showWarnings = FALSE)

          overlays_from_folders(
            traj_data_file = file.path( params$extracted_dir, bemovi_dir, bc$merged.data.folder, bc$master ),
            avi_folder = file.path( params$pre_processed_folder, bemovi_dir),
            bemovi_extract_yml_file = file.path(params$extracted_dir, bemovi_dir, bemovi_config),
            temp_overlay_folder = temp_overlay_folder,
            overlay_folder = overlay_folder,
            overlay_type = "both",
            label = "species",
            ffmpeg = "ffmpeg",
            font_size = 24,
            circle_size = 120,
            crf = 23,
            gamma = 2,
            mc_cores = params$cores,
            overwrite = overwrite
          )

        },
        bemovi_dir = yml$bemovi_dir
      )
    }
  )
  return(result)
}
