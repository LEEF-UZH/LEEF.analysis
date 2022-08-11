#' Extract timestamp from \code{sample_metadata.yml} file
#'
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{9.raw,data}
#'   are located
#'
#' @return The extracted timestamp as a string
#' @export
#'
sanity_get_timestamp <- function(
  sample_dir = "."
){
  file <- file.path(sample_dir, "00.general.parameter", "sample_metadata.yml")
  timestamp <- "ERROR"
  try(
    {timestamp <- yaml::read_yaml(file)$timestamp},
    silent = TRUE
  )
  if (timestamp == "ERROR") {
    stop("Timestamp could not be extracted Check the file '0.general.parameter/sample_metadata.yml'!")
  }

  if (!is.integer(timestamp)) {
    stop("timestamp needs to be an integer value!")
  }

  return(timestamp)
}



#' Sanity check for 0.general.data
#'
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_general.data <- function(
  sample_dir = "."
){
  fd <- file.path(sample_dir, "00.general.parameter")
  fn <- list.files(fd, full.names = FALSE)

  required_files <- c(
    "compositions.csv",
    "experimental_design.csv",
    "sample_metadata.yml"
  )

  if (!all(required_files %in% fn)){
    return(
      paste0(
        "The folder '00.general.parameter' misses the required file(s): '",
        required_files[!all(required_files %in% fn)],
        "'!"
      )
    )
  }
  return(TRUE)
}

#' Sanity check for bemovi.mag.16
#'
#' @param sample_dir root dir in which the folder \code{bemovi.mag.16} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_bemovi.mag.16 <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)

  fd <- file.path(sample_dir, "0.raw.data", "bemovi.mag.16")
  fn <- list.files(fd, full.names = FALSE)

  classifiers <- NULL
  if ("bemovi_extract.mag.16.yml" %in% fn) {
    p <- yaml::read_yaml(file.path(fd, "bemovi_extract.mag.16.yml"))
    classifiers <- c(
      p$classifier_constant,
      p$classifier_increasing
    )
  }

  required_files <- c(
    paste0(
      timestamp,
      c(
        "_00091.cxd",
        "_00092.cxd",
        "_00093.cxd",
        "_00094.cxd",
        "_00095.cxd",
        "_00096.cxd",
        "_00097.cxd",
        "_00098.cxd",
        "_00099.cxd",
        "_00100.cxd",
        "_00101.cxd",
        "_00102.cxd",
        "_00103.cxd",
        "_00104.cxd",
        "_00105.cxd",
        "_00106.cxd",
        "_00107.cxd",
        "_00108.cxd",
        "_00109.cxd",
        "_00110.cxd",
        "_00111.cxd",
        "_00112.cxd",
        "_00113.cxd",
        "_00114.cxd",
        "_00115.cxd",
        "_00116.cxd",
        "_00117.cxd",
        "_00118.cxd",
        "_00119.cxd",
        "_00120.cxd",
        "_00121.cxd",
        "_00122.cxd",
        "_00123.cxd",
        "_00124.cxd",
        "_00125.cxd",
        "_00126.cxd",
        "_00127.cxd",
        "_00128.cxd",
        "_00129.cxd",
        "_00130.cxd",
        "_00131.cxd",
        "_00132.cxd",
        "_00133.cxd",
        "_00134.cxd",
        "_00135.cxd",
        "_00136.cxd",
        "_00137.cxd",
        "_00138.cxd",
        "_00139.cxd",
        "_00140.cxd",
        "_00141.cxd",
        "_00142.cxd",
        "_00143.cxd",
        "_00144.cxd",
        "_00145.cxd",
        "_00146.cxd",
        "_00147.cxd",
        "_00148.cxd",
        "_00149.cxd",
        "_00150.cxd",
        "_00151.cxd",
        "_00152.cxd",
        "_00153.cxd",
        "_00154.cxd",
        "_00155.cxd",
        "_00156.cxd",
        "_00157.cxd",
        "_00158.cxd",
        "_00159.cxd",
        "_00160.cxd",
        "_00161.cxd",
        "_00162.cxd",
        "_00163.cxd",
        "_00164.cxd",
        "_00165.cxd",
        "_00166.cxd",
        "_00167.cxd",
        "_00168.cxd",
        "_00169.cxd",
        "_00170.cxd",
        "_00171.cxd",
        "_00172.cxd",
        "_00173.cxd",
        "_00174.cxd",
        "_00175.cxd",
        "_00176.cxd",
        "_00177.cxd",
        "_00178.cxd",
        "_00179.cxd",
        "_00180.cxd"
      )
    ),
    "bemovi_extract.mag.16.yml",
    "video.description.txt",
    classifiers
  )

  if (!all(required_files %in% fn)){
    return(
      paste0(
        "The folder 'bemovi.mag.16' misses the requred file(s): '",
        required_files[!(required_files %in% fn)],
        "'!"
      )
    )
  }

  fn <- file.path(fd, required_files)

  vd <- read.table(file.path(fd, "video.description.txt"), header = TRUE)

  if (!all(gsub("\\.cxd$", "", required_files[1:90]) %in% vd$file)) {
    return("There is an error in the 'video.description.txt' file in the 'file' column!")
  }

  vd_dates <- unique(vd$date)
  if (length(vd_dates) != 1) {
    return("The 'date' column in the 'video.description.txt' contains more than one value!")
  }

  if (vd_dates != format(as.Date(as.character(timestamp), format = "%Y%m%d"), "%d.%m.%y")) {
    return("The 'date' column in the 'video.description.txt' contains more the wrong date!")
  }


  mod_time <- unique(format(x = file.mtime(fn[1:90]), "%Y%m%d"))
  if (length(mod_time) != 1) {
    return("The videos are recorded at different dates (modification dates)!")
  }

  if (mod_time != timestamp) {
    return("The modification dates do not correspond with the timestamp in '00.general/parameter'!")
  }

  return(TRUE)
}


#' Sanity check for bemovi.mag.25
#'
#' @param sample_dir root dir in which the folder \code{bemovi.mag.25} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_bemovi.mag.25 <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)

  fd <- file.path(sample_dir, "0.raw.data", "bemovi.mag.25")
  fn <- list.files(fd, full.names = FALSE)

  classifiers <- NULL
  if ("bemovi_extract.mag.25.cropped.yml" %in% fn) {
    p <- yaml::read_yaml(file.path(fd, "bemovi_extract.mag.25.cropped.yml"))
    classifiers <- c(
      p$classifier_constant,
      p$classifier_increasing
    )
  }

  if ("bemovi_extract.mag.25.non_cropped.yml" %in% fn) {
    p <- yaml::read_yaml(file.path(fd, "bemovi_extract.mag.25.non_cropped.yml"))
    classifiers <- c(
      classifiers,
      p$classifier_constant,
      p$classifier_increasing
    )
  }

  if ("bemovi_extract.mag.25.cropped.yml" %in% fn) {
    p <- yaml::read_yaml(file.path(fd, "bemovi_extract.mag.25.cropped.yml"))
    classifiers <- c(
      classifiers,
      p$classifier_constant,
      p$classifier_increasing
    )
  }

  required_files <- c(
    paste0(
      timestamp,
      c(
        "_00001.cxd",
        "_00002.cxd",
        "_00003.cxd",
        "_00004.cxd",
        "_00005.cxd",
        "_00006.cxd",
        "_00007.cxd",
        "_00008.cxd",
        "_00009.cxd",
        "_00010.cxd",
        "_00011.cxd",
        "_00012.cxd",
        "_00013.cxd",
        "_00014.cxd",
        "_00015.cxd",
        "_00016.cxd",
        "_00017.cxd",
        "_00018.cxd",
        "_00019.cxd",
        "_00020.cxd",
        "_00021.cxd",
        "_00022.cxd",
        "_00023.cxd",
        "_00024.cxd",
        "_00025.cxd",
        "_00026.cxd",
        "_00027.cxd",
        "_00028.cxd",
        "_00029.cxd",
        "_00030.cxd",
        "_00031.cxd",
        "_00032.cxd",
        "_00033.cxd",
        "_00034.cxd",
        "_00035.cxd",
        "_00036.cxd",
        "_00037.cxd",
        "_00038.cxd",
        "_00039.cxd",
        "_00040.cxd",
        "_00041.cxd",
        "_00042.cxd",
        "_00043.cxd",
        "_00044.cxd",
        "_00045.cxd",
        "_00046.cxd",
        "_00047.cxd",
        "_00048.cxd",
        "_00049.cxd",
        "_00050.cxd",
        "_00051.cxd",
        "_00052.cxd",
        "_00053.cxd",
        "_00054.cxd",
        "_00055.cxd",
        "_00056.cxd",
        "_00057.cxd",
        "_00058.cxd",
        "_00059.cxd",
        "_00060.cxd",
        "_00061.cxd",
        "_00062.cxd",
        "_00063.cxd",
        "_00064.cxd",
        "_00065.cxd",
        "_00066.cxd",
        "_00067.cxd",
        "_00068.cxd",
        "_00069.cxd",
        "_00070.cxd",
        "_00071.cxd",
        "_00072.cxd",
        "_00073.cxd",
        "_00074.cxd",
        "_00075.cxd",
        "_00076.cxd",
        "_00077.cxd",
        "_00078.cxd",
        "_00079.cxd",
        "_00080.cxd",
        "_00081.cxd",
        "_00082.cxd",
        "_00083.cxd",
        "_00084.cxd",
        "_00085.cxd",
        "_00086.cxd",
        "_00087.cxd",
        "_00088.cxd",
        "_00089.cxd",
        "_00090.cxd"
      )
    ),
    "bemovi_extract.mag.25.cropped.yml",
    "bemovi_extract.mag.25.non_cropped.yml",
    "bemovi_extract.mag.25.yml",
    "video.description.txt",
    classifiers
  )

  if (!all(required_files %in% fn)){
    return(
      paste0(
        "The folder 'bemovi.mag.25' misses the requred file(s): '",
        required_files[!(required_files %in% fn)],
        "'!"
      )
    )
  }

  fn <- file.path(sample_dir, "0.raw.data", "bemovi.mag.25", required_files)

  vd <- read.table(file.path(fd, "video.description.txt"), header = TRUE)

  if (!all(gsub("\\.cxd$", "", required_files[1:90]) %in% vd$file)) {
    return("There is an error in the 'video.description.txt' file in the 'file' column!")
  }

  vd_dates <- unique(vd$date)
  if (length(vd_dates) != 1) {
    return("The 'date' column in the 'video.description.txt' contains more than one value!")
  }

  if (vd_dates != format(as.Date(as.character(timestamp), format = "%Y%m%d"), "%d.%m.%y")) {
    return("The 'date' column in the 'video.description.txt' contains more the wrong date!")
  }

  mod_time <- unique(format(x = file.mtime(fn[1:90]), "%Y%m%d"))
  if (length(mod_time) != 1) {
    return("The videos are recorded at different dates (modification dates)!")
  }

  if (mod_time != timestamp) {
    return("The modification dates do not correspond with the timestamp in '00.general/parameter'!")
  }

  return(TRUE)
}


#' Sanity check for flowcam
#'
#' @param sample_dir root dir in which the folder \code{flowcam} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_flowcam <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)

  fd <- file.path(sample_dir, "0.raw.data", "flowcam")
  fn <- list.files(fd, full.names = FALSE)

  classifiers <- NULL
  if ("flowcam.yml" %in% fn) {
    p <- yaml::read_yaml(file.path(fd, "flowcam.yml"))
    classifiers <- c(
      p$classifier_constant,
      p$classifier_increasing
    )
  }

  required_files <- c(
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18",
    "19",
    "20",
    "21",
    "22",
    "23",
    "24",
    "25",
    "26",
    "27",
    "28",
    "29",
    "30",
    "flowcam_dilution.csv",
    "flowcam.yml",
    classifiers
  )

  if (!all(required_files %in% fn)){
    return(
      paste0(
        "The folder 'flowcam' misses the requred file(s): '",
        required_files[!(required_files %in% fn)],
        "'!"
      )
    )
  }

  fn <- file.path(sample_dir, "0.raw.data", "flowcam", required_files)

  mod_time <- unique(format(x = file.mtime(fn[1:30]), "%Y%m%d"))
  if (length(mod_time) != 1) {
    return("The folders are recorded at different dates (modification dates)!")
  }

  if (mod_time != timestamp) {
    return("The modification dates do not correspond with the timestamp in '00.general/parameter'!")
  }

  return(TRUE)
}

#' Sanity check for flowcytometer
#'
#' @param sample_dir root dir in which the folder \code{flowcam} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_flowcytometer <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)

  fd <- file.path(sample_dir, "0.raw.data", "flowcytometer")

  required_data_files <- c(
    timestamp,
    paste0(timestamp, ".ciplus")
  )

  if (!all(required_data_files %in% list.files(fd, full.names = FALSE, recursive = FALSE))){
    return(
      paste0(
        "The folder 'flowcytometer' misses the requred data file / directory: '",
        required_data_files[!all(required_data_files %in% list.files(fd, full.names = FALSE, recursive = FALSE))],
        "'!"
      )
    )
  }

  fn <- list.files(fd, full.names = FALSE, recursive = TRUE)

  required_files <- c(
    paste0(
      timestamp, "/",
      c(
        "A01.fcs",
        "A02.fcs",
        "A03.fcs",
        "A04.fcs",
        "A05.fcs",
        "A06.fcs",
        "A07.fcs",
        "A08.fcs",
        "A09.fcs",
        "A10.fcs",
        "A11.fcs",
        "A12.fcs",
        "B01.fcs",
        "B02.fcs",
        "B03.fcs",
        "B04.fcs",
        "B05.fcs",
        "B06.fcs",
        "B07.fcs",
        "B08.fcs",
        "B09.fcs",
        "B10.fcs",
        "B11.fcs",
        "B12.fcs",
        "C01.fcs",
        "C02.fcs",
        "C03.fcs",
        "C04.fcs",
        "C05.fcs",
        "C06.fcs",
        "C07.fcs",
        "C08.fcs",
        "C09.fcs",
        "C10.fcs",
        "C11.fcs",
        "C12.fcs",
        "D01.fcs",
        "D02.fcs",
        "D03.fcs",
        "D04.fcs",
        "D05.fcs",
        "D06.fcs",
        "D07.fcs",
        "D08.fcs",
        "D09.fcs",
        "D10.fcs",
        "D11.fcs",
        "D12.fcs",
        "E01.fcs",
        "E02.fcs",
        "E03.fcs",
        "E04.fcs",
        "E05.fcs",
        "E06.fcs",
        "E07.fcs",
        "E08.fcs",
        "E09.fcs",
        "E10.fcs",
        "E11.fcs",
        "E12.fcs",
        "F01.fcs",
        "F02.fcs",
        "F03.fcs",
        "F04.fcs",
        "F05.fcs",
        "F06.fcs",
        "F07.fcs",
        "F08.fcs",
        "F09.fcs",
        "F10.fcs",
        "F11.fcs",
        "F12.fcs",
        "G01.fcs",
        "G02.fcs",
        "G03.fcs",
        "G04.fcs",
        "G05.fcs",
        "G06.fcs",
        "G07.fcs",
        "G08.fcs",
        "G09.fcs",
        "G10.fcs",
        "G11.fcs",
        "G12.fcs",
        "H01.fcs",
        "H02.fcs",
        "H03.fcs",
        "H04.fcs",
        "H05.fcs",
        "H06.fcs",
        "H07.fcs",
        "H08.fcs",
        "H09.fcs",
        "H10.fcs",
        "H11.fcs",
        "H12.fcs"
      )
    ),
    paste0(timestamp, ".ciplus"),
    "gates_coordinates.csv",
    "metadata_flowcytometer.csv"
  )

  if (!all(required_files %in% fn)){
    return(
      paste0(
        "The folder 'flowcam' misses the requred file(s): '",
        required_files[!all(required_files %in% fn)],
        "'!"
      )
    )
  }

  fn <- file.path(sample_dir, "0.raw.data", "flowcytometer", required_files)

  mod_time <- unique(format(x = file.mtime(fn[1:30]), "%Y%m%d"))
  if (length(mod_time) != 1) {
    return("The folders are recorded at different dates (modification dates)!")
  }

  if (mod_time != timestamp) {
    return("The modification dates do not correspond with the timestamp in '00.general/parameter'!")
  }

  return(TRUE)
}



#' Sanity check for manualcount
#'
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_manualcount <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)
  fd <- file.path(sample_dir, "0.raw.data", "manualcount")
  fn <- list.files(fd, full.names = TRUE)

  if (length(fn) != 1) {
    return("Either manualcount file is missing or more than one file exists. Exactly one expected!")
  }

  if (basename(fn)[[1]] != "manual_count.xlsx") {
    return("Wrong file name. The file has to be named 'manual_count.xlsx'!")
  }

  mod_time <- format(x = file.mtime(fn[[1]]), "%Y%m%d")
  if (mod_time != timestamp) {
    return("The timestamp is not equal to the modification time of the o2meter file!")
  }
  return(TRUE)
}





#' Sanity check for o2meter
#'
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return \code{TRUE} if the test passed, a string indicating the error if it failed.
#' @export
#'
sanity_check_o2meter <- function(
  sample_dir = "."
){
  timestamp <- sanity_get_timestamp(sample_dir)
  fd <- file.path(sample_dir, "0.raw.data", "o2meter")
  fn <- list.files(fd, full.names = TRUE)

  if (length(fn) != 1) {
    return("Either o2meter file is missing or more than one file exists. Exactly one expected!")
  }

  mod_time <- format(x = file.mtime(fn), "%Y%m%d")
  if (mod_time != timestamp) {
    return("The timestamp is not equal to the modification time of the o2meter file!")
  }

  return(TRUE)
}




#' Sanity check data for pipeline
#'
#' Executes onle test per method required.
#' @param sample_dir root dir in which the folder \code{00.general.parameter} and \code{0.raw.data}
#'   are located
#'
#' @return named \code{list} object, with one element per method. If the test passed
#'   for the method, the result is \code{TRUE}, if it fails, a string indicating the
#'   reason why it failed
#' @export
#'
sanity_check <- function(
  sample_dir = "."
){
  result <- list(
    general.parameter = sanity_check_general.data(sample_dir),
    bemovi.mag.16 = sanity_check_bemovi.mag.16(sample_dir),
    bemovi.mag.25 = sanity_check_bemovi.mag.25(sample_dir),
    flowcam = sanity_check_flowcam(sample_dir),
    flowcytometer = sanity_check_flowcytometer(sample_dir),
    manualcount = sanity_check_manualcount(sample_dir),
    o2meter = sanity_check_o2meter(sample_dir)
  )
  return(result)
}
