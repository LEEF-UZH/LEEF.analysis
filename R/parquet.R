parquet_add_bemovi_16 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("mean.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_16_density.parquet")
            } else if (grepl("morph.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_16_morph.parquet")
            } else {
                stop("file name does not match any of the expected patterns")
            }

            if (!dir.exists(path_to_parquet)) {
                dir.create(path_to_parquet)
            }
            parquetize::rds_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "bottle", "species")
            )
        }
    )
}

parquet_add_bemovi_25 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")

            if (grepl("mean.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_density.parquet")
            } else if (grepl("morph.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_morph.parquet")
            } else {
                stop("file name does not match any of the expected patterns")
            }

            if (!dir.exists(path_to_parquet)) {
                dir.create(path_to_parquet)
            }

            parquetize::rds_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "bottle", "species")
            )
        }
    )
}

parquet_add_flowcam <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("algae_density.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcam_density.parquet")
            } else if (grepl("algae_traits.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcam_traits.parquet")
            } else {
                stop("file name does not match any of the expected patterns")
            }

            if(!dir.exists(path_to_parquet)){
                dir.create(path_to_parquet)
            }
            parquetize::rds_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "bottle", "species")
            )
        }
    )
}

parquet_add_flowcytometer <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("flowcytometer_density.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcytometer_density.parquet")
            } else if (grepl("flowcytometer_traits.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcytometer_traits.parquet")
            } else {
                stop("file name does not match any of the expected patterns")
            }

            if (!dir.exists(path_to_parquet)) {
                dir.create(path_to_parquet)
            }
            parquetize::rds_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "bottle", "species")
            )
        }
    )
}

# generate roxygen documentation of the following function
parquet_add_o2 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            
            path_to_parquet <- file.path(path_to_parquet_root_dir, "o2")

            if (!dir.exists(path_to_parquet)) {
                dir.create(path_to_parquet)
            }
            parquetize::csv_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "bottle")
            )
        }
    )
}
