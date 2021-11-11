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
#'   \item \strong{CORTEX} (\code{.xlsx} or \code{.xls} files in German
#'     language)
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
  if (is.null(device)) device <- guess_device(file)
  switch(device,
         zan = spiro_import_zan(file),
         cosmed = spiro_import_cosmed(file),
         cortex = spiro_import_cortex(file),
         stop("Could not find device type. Please specify the 'type' argument")
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
  meta_imin <- which(rawdata == "[person]") # meta data
  cnames_imin <- which(rawdata == "[parameter]") # column names
  data_imin <- which(rawdata == "[Data]") # raw data

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
  # Replace missing values with NAs
  info <- replace(info,which(info == ""),NA)

  # import column names for main data structure
  cnames <- utils::read.csv(file, header = FALSE,
                            skip = cnames_imin+2,
                            nrows = data_imin-cnames_imin-4)$V3
  # remove last column due to encoding problems
  cnames <- cnames[-length(cnames)]

  # import the main data
  data <- utils::read.csv(file, header = FALSE,
                          skip = data_imin,
                          col.names = c("index",cnames,"fan"))

  # -- TO DO --
  # load import currently only works for velocity (not for power)
  # import works only for files of German language

  # write a data frame for the main parameters
  df <- data.frame(
    time = data$Zeit/1000, # time is given in milliseconds in the raw data
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    # length of inhalation plus exhalation in milliseconds gives respiratory
    # rate
    RR = 60000 / (data$tin + data$tex),
    # ventilation (per minute) is given in ml in the raw data
    VT = (data$Vin / 1000),
    VE = (60 * data$Vin) / (data$tin + data$tex), # calculate minute ventilation
    HR = data$HR,
    # velocity is given in m/min in the raw data
    load = round(data$Geschw./3600,2),
    #incr = data$Steig./10, # variable currently not used
    PetO2 = NA,
    PetCO2 = NA
  )

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro","data.frame") # create spiro class
  df
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
  if (grepl("\\.xls", file)) { #Excel file
    # Read head of the Excel file
    head <- readxl::read_excel(file, range = "A1:B4", col_names = c("V1","V2"))

    # files from Cosmed devices usually start with a line "ID-Code:"
    if (any(head == "ID code:", na.rm = TRUE)|
        any(head == "ID-Code:", na.rm = TRUE)) {
      device <- "cosmed"
    # files from Cortex devices usually contain a line at the head: "Bediener"
    # -- TO DO --
    # this is currently only working in German language
    # English equivalent is needed
    } else if (any(head == "Bediener", na.rm = TRUE)) {
      device <- "cortex"
    } else { # device type not found
      device <- "none"
    }
  } else { # non-Excel file
    # read the first rows of the file
    head <- utils::read.delim(file, header = FALSE, nrows = 5)
    # files from ZAN devices usually start with a line "[person]"
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

  # read meta data
  tbl <- suppressMessages(
    readxl::read_excel(file, range = "A1:B8", col_names = FALSE))
  # write as data frame
  ldf <- data.frame(t(as.data.frame(tbl[[2]], row.names = tbl[[1]])))

  # search meta data by names of the desired language
  if (tbl[[1]][[2]] == "Nachname:") { # for German language
    name <- "Vorname."
    surname <- "Nachname."
    age <- "Alter."
    sex <- "Geschlecht."
    height <- "Gr\u00f6\u00dfe..cm.." # special handling for umlaute
    weight <- "Gewicht..Kg.."
  } else { # for English language
    name <- "First.name."
    surname <- "Last.name."
    age <- "Age."
    sex <- "Sex."
    height <- "Height..cm.."
    weight <- "Weight..Kg.."
  }

  # find and write meta data
  info <- data.frame(
    name = ldf[[name]],
    surname = ldf[[surname]],
    birthday = ldf[[age]],
    sex = get_sex(ldf[[sex]]),
    height = as.numeric(ldf[[height]]),
    weight = as.numeric(ldf[[weight]])
  )

  # read main data
  data <- readxl::read_excel(file, range = readxl::cell_cols(10:50))[-1:-2,]
  # convert time to seconds (integer)
  l <- to_seconds(data$t)
  # remove rows with no time specified. These might occur at the end of the
  # file
  data <- data[l != 0,]

  # -- TO DO --
  # rewrite search for speed and grade column
  # search for power column

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

  # write data
  df <- data.frame(
    time = l[l != 0], # exclude not specified times
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    RR = data$Rf,
    VT = data$VT,
    VE = data$VE,
    HR = data$HR,
    load = round(speed/36,2),
    #incr = grade, # variable currently not used
    PetO2 = data$PetO2,
    PetCO2 = data$PetCO2
  )

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  # rare special case if weight has been deleted from meta data. Recalculates
  # weight based on relative oxygen uptake present in raw data
  if (is.na(info$weight)) {
    info$weight <- round(
      # used first data value to recalculate weight
      as.numeric(data$VO2[[1]]) / as.numeric(data$`VO2/Kg`[[1]]),1)
  }

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro","data.frame") # create spiro class
  df
}

#' Import raw data from Cortex spiroergometric devices
#'
#' \code{spiro_import_cortex()} retrieves cardiopulmonary data from cortex
#' metabolic cart files.
#'
#' @param file A character string, giving the path of the data file.
#'
#' @return A \code{data.frame} with data. The attribute \code{info} contains
#'   addition meta-data retrieved from the original file.
spiro_import_cortex <- function(file) {
  # read excel file
  d <- suppressMessages(
    readxl::read_excel(file, col_names = FALSE)
  )

  # -- TO DO --
  # create import option for files of English language

  # get meta data (German language)
  name <- get_meta(d,"Name",3)
  surname <- get_meta(d,"Vorname",3)
  sex <- get_meta(d,"Geschlecht",3)
  birthday <- get_meta(d,"Geburtsdatum",3)
  height <- get_meta(d,"Gr\u00f6\u00dfe",3) # special handling for umlaute
  weight <- get_meta(d,"Gewicht",3)

  # write data frame for metadata
  info <- data.frame(name,
                     surname,
                     birthday,
                     sex = get_sex(sex),
                     height = to_number(height),
                     weight = to_number(weight)
  )

  # get start of data section
  t_ind <- which(d[,1] == "t")

  # get parameter labels
  cols <- as.character(d[t_ind,])

  # get parameter count
  coln <- sum(!is.na(cols))

  # get raw data
  data <- d[(t_ind+2):nrow(d),1:coln]
  names(data) <- cols[1:coln]

  # handle missing parameters
  if (any(names(data) != "Geschwindigkeit")) {
    data$Geschwindigkeit <- 0
  }
  if (any(names(data) != "Steigung")) {
    data$Steigung <- 0
  }

  # in some cases VCO2 may be missing and thus is recalculated from RER and VO2
  if (all(names(data) != "V'CO2", na.rm = TRUE)) {
    data$`V'CO2` <- as.numeric(data$`V'O2`) / as.numeric(data$RER)
  }

  # checking for different variable naming
  if (any(names(data) == "V'E (BTPS)", na.rm = TRUE)) {
    ve_name <- "V'E (BTPS)"
    vo2_name <- "V'O2 (STPD)"
  } else {
    ve_name <- "V'E"
    vo2_name <- "V'O2"
  }

  df <- data.frame(
    # use first column for time independent of name
    time = to_seconds(data[[1]]),
    VO2 = as.numeric(data[[vo2_name]]),
    VCO2 = as.numeric(data$`V'CO2`),
    RR = as.numeric(data$AF),
    VT = as.numeric(data$VT),
    VE = as.numeric(data[[ve_name]]),
    HR = as.numeric(data$HF),
    load = as.numeric(data$Steigung),
    #incr = as.numeric(data$Steigung), # variable currently not used
    PetO2 = as.numeric(data$PetO2),
    PetCO2 = as.numeric(data$PetCO2)
  )

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro","data.frame") # create spiro class
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
  # data for seconds might contain decimals. Replace comma by point as decimal
  # mark (e.g. hh:mm:ss,s -> hh:mm:ss.s)
  time <- gsub(",",".",time)
  # split time by double colon separator
  split <- strsplit(time, ":")[[1]]
  time_split <- as.numeric(split) # convert vectors to numbers

  # calculate time in seconds based on input format
  if (length(time_split) == 3) {
    # for hh:mm:ss or hh:mm:ss.s
    s <- 3600*time_split[[1]]+60*time_split[[2]]+time_split[[3]]
  } else if (length(time_split) == 2) {
    # for mm:ss
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
  out <- factor(sex, levels = c("female","male","diverse"))
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
  as.numeric(gsub("[^\\.0-9]","",gsub(",",".",chr)))
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
  if (any(s,na.rm = TRUE)) {
    out <- data[[max(s),column]]
  } else {
    out <- NA
  }
  out
}
