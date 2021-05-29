#' Import raw data from spiroergometric devices
#'
#' \code{spiro_import()} retrieves cardiopulmonary data from various types of
#' metabolic cart files.
#'
#' Different metabolic carts yield different output formats for their data. By
#' default, this function will guess the used device based on the
#' characteristics of the given file. This behaviour can be overridden by
#' explicitly stating \code{device}.
#'
#' The currently supported metabolic carts are:
#' \itemize{
#'   \item \strong{COSMED} (\code{.xlsx} or \code{.xls} files, either in English
#'     or German language)
#'   \item \strong{ZAN} (\code{.dat} files in German language, usually with
#'     names in the form of \code{"EXEDxxx"})
#' }
#'
#' @inheritParams spiro
#'
#' @return A \code{data.frame} with data. The attribute \code{info} contains
#'   addition meta-data retrieved from the original file.
#'
#' @examples
#' # Get example data
#' file <- spiro_example("zan_gxt")
#'
#' spiro_import(file)
#' @export

spiro_import <- function(file, device = NULL) {
  file <- get_path(file)
  if (is.null(device)) device <- guess_device(file)
  switch(device,
         zan = spiro_import_zan(file),
         cosmed = spiro_import_cosmed(file),
         stop("'type' not specified")
  )
}

#' Import raw data from ZAN spiroergometric devices
#'
#' \code{spiro_import_zan()} retrieves cardiopulmonary data from ZAN
#' metabolic cart files.
#'
#' @param file A character string, giving the path of the data file.
#'
#' @return A \code{data.frame} with data. The attribute \code{info} contains
#'   addition meta-data retrieved from the original file.
spiro_import_zan <- function(file) {

  # find indices for document structure
  rawdata <- utils::read.delim(file, header = FALSE, blank.lines.skip = FALSE)
  meta_imin <- which(rawdata == "[person]")
  cnames_imin <- which(rawdata == "[parameter]")
  data_imin <- which(rawdata == "[Data]")

  # import meta data
  meta <- utils::read.csv(file, sep = "=",
                          header = FALSE,
                          skip = meta_imin,
                          nrows = cnames_imin-meta_imin-3,
                          row.names = 1,
                          blank.lines.skip = FALSE)
  meta_df <- data.frame(t(meta))
  info <- data.frame(
    name = meta_df$vorname,
    surname = meta_df$name,
    birthday = meta_df$geburtstag,
    sex = get_sex(meta_df$geschlecht),
    height = as.numeric(meta_df$groesse),
    weight = as.numeric(meta_df$gewicht)
  )

  # import column names for main data structure
  cnames <- utils::read.csv(file, header = FALSE,
                            skip = cnames_imin+2,
                            nrows = data_imin-cnames_imin-4)$V3
  # remove column due to encoding problems
  cnames <- cnames[-length(cnames)]

  # import the main data
  data <- utils::read.csv(file, header = FALSE,
                          skip = data_imin,
                          col.names = c("index",cnames,"fan"))
  df <- data.frame(
    time = data$Zeit/1000,
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    RR = 60000 / (data$tin + data$tex),
    VT = (data$Vin / 1000),
    VE = (60 * data$Vin) / (data$tin + data$tex),
    HR = data$HR,
    velocity = round(data$Geschw./3600,2),
    incr = data$Steig./10
  )

  attr(df, "info") <- info
  class(df) <- c("spiro","data.frame")
  df
}

#' Get the file path for a data file
#'
#' \code{get_path()} performs a serch for a data file.
#'
#' Considering the input to be a regular expression, this function will search
#' one level above the current working directory to find matching files.
#'
#' If no matching files are found, \code{get_path} takes the input as a direct
#' path to a file. If more than one match is found, an error will be displayed.
#'
#' @param name A character string, giving the file name to look for.
#'
#' @return A character string containing the file path.

get_path <- function(name) {
  filepath <- list.files(path = "..",
                         pattern = name,
                         recursive = TRUE,
                         full.names = TRUE)
  if (length(filepath) >1 ) {
    stop("More than one file was found matching the input")
  } else if (length(filepath) != 1) {
    filepath <- name
  }
  filepath
}


#' Guess the device used for a cardiopulmonary measurement
#'
#' \code{guess_device()} guesses the device type of a metabolic cart based on
#' the characteristics of a raw data file. To get information on supported
#' devices visit \code{\link{spiro_import}}.
#'
#' @param file A character string, giving the path of the data file.
#'
#' @return A character string specifying the guessed device.

guess_device <- function(file) {
  if (grepl("\\.xls", file)) {
    head <- readxl::read_excel(file, range = "A1:B4", col_names = c("V1","V2"))
    if (any(head == "ID code:")| any(head == "ID-Code:")) {
      device <- "cosmed"
    } else {
      device <- "none"
    }
  } else {
    head <- utils::read.delim(file, header = FALSE, nrows = 10)
    if (any(head == "[person]")) device <- "zan" else device <- "none"
  }
  device
}

#' Import raw data from COSMED spiroergometric devices
#'
#' \code{spiro_import_cosmed()} retrieves cardiopulmonary data from ZAN
#' metabolic cart files.
#'
#' @param file A character string, giving the path of the data file.
#'
#' @return A \code{data.frame} with data. The attribute \code{info} contains
#'   addition meta-data retrieved from the original file.
spiro_import_cosmed <- function(file) {
  tbl <- suppressMessages(
    readxl::read_excel(file, range = "A1:B8", col_names = FALSE))
  ldf <- data.frame(t(as.data.frame(tbl[[2]], row.names = tbl[[1]])))

  if (tbl[[1]][[2]] == "Nachname:") { # For German language
    name <- "Vorname."
    surname <- "Nachname."
    age <- "Alter."
    sex <- "Geschlecht."
    height <- "Gr\u00f6\u00dfe..cm.."
    weight <- "Gewicht..Kg.."
  } else { # For English language
    name <- "First.name."
    surname <- "Last.name."
    age <- "Age."
    sex <- "Sex."
    height <- "Height..cm.."
    weight <- "Weight..Kg.."
  }

  info <- data.frame(
    name = ldf[[name]],
    surname = ldf[[surname]],
    birthday = ldf[[age]],
    sex = get_sex(ldf[[sex]]),
    height = as.numeric(ldf[[height]]),
    weight = as.numeric(ldf[[weight]])
  )

  data <- readxl::read_excel(file, range = readxl::cell_cols(10:50))[-1:-2,]
  l <- to_seconds(data$t)
  data <- data[l != 0,]

  suppressWarnings(
    if (is.null(data$Speed)) {
      speed <- rep.int(0,length(l[l != 0]))
    } else {
      speed <- as.numeric(data$Speed)
    }
  )
  suppressWarnings(
    if (is.null(data$Grade)) {
      grade <- rep.int(0,length(l[l != 0]))
    } else {
      grade <- as.numeric(data$Grade)
    }
  )
  df <- data.frame(
    time = l[l != 0],
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    RR = data$Rf,
    VT = data$VT,
    VE = data$VE,
    HR = data$HR,
    velocity = round(speed/36,2),
    incr = grade
  )
  if (is.na(info$weight)) {
    info$weight <- round(
      as.numeric(data$VO2[[1]]) / as.numeric(data$`VO2/Kg`[[1]]),1)
  }

  attr(df, "info") <- info
  class(df) <- c("spiro","data.frame")
  df
}

#' Convert time data to seconds
#'
#' \code{to_seconds()} converts time data of the form hh:mm:ss to seconds.
#'
#' @param time_data A character vector containing the time data in the format
#'   hh:mm:ss or mm:ss.
to_seconds <- function(time_data) {
  sapply(time_data, to_seconds.internal, USE.NAMES = FALSE)
}

#' Convert one time data to seconds
#'
#' \code{to_seconds.internal()} is an internal function to
#' \code{\link{to_seconds}}
#'
#' @param time_data A character string containing the time data in the format
#'   hh:mm:ss or mm:ss.
#' @noRd
to_seconds.internal <- function(time) {
  split <- strsplit(time, ":")[[1]]
  split[[length(split)]] <- gsub(",",".",split[[length(split)]])
  time_split <- as.numeric(split)
  if (length(time_split) == 3) {
    s <- 3600*time_split[[1]]+60*time_split[[2]]+time_split[[3]]
  } else if (length(time_split) == 2) {
    s <- 60*time_split[[1]]+time_split[[2]]
  }
  s
}

#' Convert sex to factor level
#'
#' \code{get_sex()} is a helper function to retrieve the correct sex from file
#' metadata.
#'
#' @param chr A character string containing information on the participant's sex
#'   as specified in the raw data file metadata.
#'
#' @return A factor level, either \code{male} or \code{female}.
#' @noRd
get_sex <- function(chr) {
  sex <- switch(chr,
                m = ,
                "M" = ,
                "m\u00e4nnlich" = ,
                male = "male",
                f = ,
                "F" = ,
                w = ,
                weiblich = ,
                female = "female",
                NA)
  if (is.null(sex)) sex <- NA
  out <- factor(sex, levels = c("female","male"))
  out
}

#' Read decimal numbers from characters with comma
#'
#' \code{to_number()} is a helper function to retrieve decimal numbers from
#' characters with a comma.
#'
#' @param chr A character string containing a number. A comma might be used as
#'   decimal mark.
#'
#' @return A (decimal) number.
#' @noRd
to_number <- function(chr) {
  readr::parse_number(gsub(",",".",chr))
}

#' Get file (meta)data from a file by name
#'
#' \code{get_meta()} is a helper function to retrieve information in the same
#' data row as a given expression.
#'
#' @param data A data.frame containing the data
#' @param name An expression. The metadata's name which has to be searched for.
#' @param column A numeric value, specifying the column from which the data
#'   should be taken
#'
#' @return A character string.
#' @noRd
get_meta <- function(data,name,column) {
  s <- which(data == name)
  if (any(s)) {
    out <- data[[max(s),column]]
  } else {
    out <- NA
  }
  out
}
