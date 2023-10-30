#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_bemovi_16 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    unlink(file.path(path_to_parquet_root_dir, "bemovi_16_density"), recursive = TRUE, force = TRUE)
    unlink(file.path(path_to_parquet_root_dir, "bemovi_16_morph"), recursive = TRUE, force = TRUE)

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

            object <- readRDS(fn)
            if (rename) {
                object <- LEEF_2_rename_species(object)
                object <- LEEF_2_rename_species_prob_columns(object)
            } else {
                names(object) <- tolower(names(object))
            }

            object_to_parquet(
                object = object,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp"),
                compression = compression
            )
        }
    )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_bemovi_25 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    unlink(file.path(path_to_parquet_root_dir, "bemovi_25_density"), recursive = TRUE, force = TRUE)
    unlink(file.path(path_to_parquet_root_dir, "bemovi_25_morph"), recursive = TRUE, force = TRUE)

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

            object <- readRDS(fn)
            if (rename) {
                object <- LEEF_2_rename_species(object)
                object <- LEEF_2_rename_species_prob_columns(object)
            } else {
                names(object) <- tolower(names(object))
            }

            object_to_parquet(
                object = object,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp"),
                compression = compression
            )
        }
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_bemovi_25_cropped <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    unlink(file.path(path_to_parquet_root_dir, "bemovi_25_density_cropped"), recursive = TRUE, force = TRUE)
    unlink(file.path(path_to_parquet_root_dir, "bemovi_25_morph_cropped"), recursive = TRUE, force = TRUE)

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

            object <- readRDS(fn)
            if (rename) {
                object <- LEEF_2_rename_species(object)
                object <- LEEF_2_rename_species_prob_columns(object)
            } else {
                names(object) <- tolower(names(object))
            }

            object_to_parquet(
                object = object,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp"),
                compression = compression
            )
        }
    )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_flowcam <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    unlink(file.path(path_to_parquet_root_dir, "flowcam_density"), recursive = TRUE, force = TRUE)
    unlink(file.path(path_to_parquet_root_dir, "flowcam_traits"), recursive = TRUE, force = TRUE)

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

            object <- readRDS(fn)
            if (rename) {
                object <- LEEF_2_rename_species(object)
                object <- LEEF_2_rename_species_prob_columns(object)
            } else {
                names(object) <- tolower(names(object))
            }

            object_to_parquet(
                object = object,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp"),
                compression = compression
            )
        }
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_flowcytometer <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    unlink(file.path(path_to_parquet_root_dir, "flowcytometer_density"), recursive = TRUE, force = TRUE)
    unlink(file.path(path_to_parquet_root_dir, "flowcytometer_traits"), recursive = TRUE, force = TRUE)

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

            object <- readRDS(fn)
            if (rename) {
                object <- LEEF_2_rename_species(object)
                object <- LEEF_2_rename_species_prob_columns(object)
            } else {
                names(object) <- tolower(names(object))
            }

            object_to_parquet(
                object = object,
                path_to_parquet = path_to_parquet,
                partition = "yes",
                partitioning = c("timestamp", "species"),
                compression = compression
            )
        }
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_o2 <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    message("\nadding  O2")

    object <- lapply(
        fns,
        readRDS
    ) |>
        do.call(what = rbind)

    if (rename) {
        names(object) <- tolower(names(object))
    } else {
        names(object) <- tolower(names(object))
    }

    path_to_parquet <- file.path(path_to_parquet_root_dir, "o2")
    unlink(file.path(path_to_parquet, recursive = TRUE, force = TRUE))
    dir.create(path_to_parquet, recursive = TRUE, showWarnings = FALSE)

    object_to_parquet(
        object = object,
        path_to_parquet = path_to_parquet,
        partition = "no",
        compression = compression
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fns PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
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
#' @rdname parquet
#' @export
#' @importFrom pbapply pblapply
parquet_add_manualcount <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    message("\nadding  Manualcount")

    object <- lapply(
        fns,
        read.csv
    ) |>
        do.call(what = rbind)

    if (rename) {
        object <- LEEF_2_rename_species(object)
        object <- LEEF_2_rename_species_prob_columns(object)
    } else {
        names(object) <- tolower(names(object))
    }

    path_to_parquet <- file.path(path_to_parquet_root_dir, "manualcount", "")
    unlink(path_to_parquet, recursive = TRUE, force = TRUE)
    dir.create(path_to_parquet, recursive = TRUE, showWarnings = FALSE)

    object_to_parquet(
        object = object,
        path_to_parquet = path_to_parquet,
        partition = "no",
        compression = compression
    )
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param compression PARAM_DESCRIPTION, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#' @rdname parquet
#' @export
parquet_add_conductivity <- function(
    fns = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    message("\nadding  Conductivity")

    object <- lapply(
        fns,
        read.csv
    ) |>
        do.call(what = rbind)

    if (rename) {
        names(object) <- tolower(names(object))
    } else {
        names(object) <- tolower(names(object))
    }

    path_to_parquet <- file.path(path_to_parquet_root_dir, "conductivity")
    unlink(file.path(path_to_parquet, recursive = TRUE, force = TRUE))
    dir.create(path_to_parquet, recursive = TRUE, showWarnings = FALSE)

    object_to_parquet(
        object = object,
        path_to_parquet = path_to_parquet,
        partition = "no",
        compression = compression
    )
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fn PARAM_DESCRIPTION, Default: NULL
#' @param path_to_parquet_root_dir PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param rename Should data be renamed to follow specified standards, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#' @rdname parquet
#' @export
parquet_add_toc <- function(
    fn = NULL,
    path_to_parquet_root_dir = NULL,
    compression = "snappy",
    rename = FALSE) {
    message("\nadding  TOC")

    object <- readRDS(fn)
    if (rename) {
        object <- LEEF_2_rename_toc(object)
    } else {
        names(object) <- tolower(names(object))
    }

    path_to_parquet <- file.path(path_to_parquet_root_dir, "toc")
    unlink(file.path(path_to_parquet, recursive = TRUE, force = TRUE))
    dir.create(path_to_parquet, recursive = TRUE, showWarnings = FALSE)

    object_to_parquet(
        object = object,
        path_to_parquet = file.path(path_to_parquet_root_dir, "toc"),
        partition = "no",
        compression = compression
    )
}


#' FUNCTION_TITLE
#'
#' Essentially combined re-implementation of \code{\link[parquetize]{rds_to_parquet}}
#' and \code{\link[parquetize]{csv_to_parquet}} plus standardisation on small
#' letter column names and timestamps as character.
#' @param object object to be written as a parquet file / arrow db
#' @param path_to_parquet PARAM_DESCRIPTION, Default: NULL
#' @param compression compression as used in \code{\link[arrow]{write_parquet}}, Default: "snappy"
#' @param compression_level PARAM_DESCRIPTION, Default: "NULL"
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @importFrom parquetize write_parquet_at_once
#' @examples
#' \dontrun{
#' if (interactive()) {
#'     # EXAMPLE1
#' }
#' }
#' @seealso
#' @rdname parquet
#' @export
object_to_parquet <- function(
    object,
    path_to_parquet,
    partition = "no",
    compression = "snappy",
    compression_level = NULL,
    ...) {
    if (missing(path_to_parquet)) {
        stop("Missing required argument `path_to_parquet`!")
    }

    ### some data standardisation
    names(object) <- tolower(names(object))
    if ("timestamp" %in% names(object)) {
        object$timestamp <- as.character(object$timestamp)
    }

    ### and write the data
    dataset <- parquetize::write_parquet_at_once(
        object,
        path_to_parquet,
        partition,
        compression,
        compression_level,
        ...
    )
    return(invisible(dataset))
}
