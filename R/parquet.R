#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sources PARAM_DESCRIPTION, Default: NULL
#' @param partitioning PARAM_DESCRIPTION, Default: NULL
#' @param hive_style PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[arrow]{open_dataset}}
#' @rdname new_parquet_definition
#' @export 
new_parquet_definition <- function(
    sources = NULL,
    partitioning = NULL,
    hive_style = TRUE) {
    def <- list(
        sources = sources,
        partitioning = partitioning,
        hive_style = hive_style
    )
    class(def) <- append(class(def), "parquet_definition")
    return(def)
}




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_bemovi_16
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize rds_to_parquet
parquet_add_bemovi_16 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("mean.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_16_density")
            } else if (grepl("morph.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_16_morph")
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
                partitioning = c("bottle", "species")
            )
        }
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_16_density"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
    "class<-"("parquet_definition") |>
    saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_16_density.arrow.rds")
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_16_density"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_16_morph.arrow.rds")
    )
}

# TODO CHECK FOR cropped etc

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_bemovi_25
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize rds_to_parquet
parquet_add_bemovi_25 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")

            if (grepl("mean.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_density")
            } else if (grepl("morph.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_morph")
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
                partitioning = c("bottle", "species")
            )
        }
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_25_density"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |>
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_25_density.arrow.rds")
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_25_morph"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_25_morph.arrow.rds")
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_bemovi_25_cropped
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize rds_to_parquet
parquet_add_bemovi_25_cropped <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")

            if (grepl("mean.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_density_cropped")
            } else if (grepl("morph.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "bemovi_25_morph_cropped")
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
                partitioning = c("bottle", "species")
            )
        }
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_25_density_cropped"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_25_density_cropped.arrow.rds")
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "bemovi_25_morph_cropped"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "bemovi_25_morph_cropped.arrow.rds")
    )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_flowcam
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize rds_to_parquet
parquet_add_flowcam <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("algae_density.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcam_density")
            } else if (grepl("algae_traits.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcam_traits")
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
                partitioning = c("bottle", "species")
            )
        }
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "flowcam_traits"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "flowcam_traits.arrow.rds")
    )

    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "flowcam_density"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "flowcam_density.arrow.rds")
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_flowcytometer
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize rds_to_parquet
parquet_add_flowcytometer <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")
            if (grepl("flowcytometer_density.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcytometer_density")
            } else if (grepl("flowcytometer_traits.*\\.rds$", fn)) {
                path_to_parquet <- file.path(path_to_parquet_root_dir, "flowcytometer_traits")
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
                partitioning = c("bottle", "species")
            )
        }
    )
    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "flowcytometer_traits"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "flowcytometer_traits.arrow.rds")
    )
    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "flowcytometer_density"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle", "species"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "flowcytometer_density.arrow.rds")
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{csv_to_parquet}}
#' @rdname parquet_add_o2
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize csv_to_parquet
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
                partitioning = c("bottle")
            )
        }
    )
    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "o2"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "o2.arrow.rds")
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[pbapply]{pbapply}}
#'  \code{\link[parquetize]{csv_to_parquet}}
#' @rdname parquet_add_manualcount
#' @export
#' @importFrom pbapply pblapply
#' @importFrom parquetize csv_to_parquet
parquet_add_manualcount <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL) {
    pbapply::pblapply(
        fns,
        function(fn) {
            message("\nadding ", basename(fn), " ...\n")

            path_to_parquet <- file.path(path_to_parquet_root_dir, "manualcount")

            if (!dir.exists(path_to_parquet)) {
                dir.create(path_to_parquet)
            }
            parquetize::csv_to_parquet(
                path_to_file = fn,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("bottle")
            )
        }
    )
    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "manualcount"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "manualcount.arrow.rds")
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[parquetize]{rds_to_parquet}}
#' @rdname parquet_add_toc
#' @export
#' @importFrom parquetize rds_to_parquet
parquet_add_toc <- function(
    fn = NULL,
    path_to_parquet_root_dir = NULL) {
    parquetize::rds_to_parquet(
        path_to_file = fn,
        path_to_parquet = file.path(path_to_parquet_root_dir, "toc"),
        partition = "yes",
        partitioning = c("bottle")
    )
    list(
        sources = list.files(
            file.path(path_to_parquet_root_dir, "toc"),
            pattern = "\\.parquet$",
            recursive = TRUE,
            full.names = TRUE
        ),
        partitioning = c("bottle"),
        hive_style = TRUE
    ) |> 
     "class<-"("parquet_definition") |> 
     saveRDS(
        file = file.path(path_to_parquet_root_dir, "toc.arrow.rds")
    )
}
