#' Import raw data from spiroergometric devices
#'
#' \code{spiro_import()} retrieves cardiopulmonary data from various types of
#' metabolic cart files.
#'
#' Different metabolic carts yield different output formats for their data. By
#' default, this function will guess the used device based on the
#' characteristics of the given file. This behavior can be overridden by
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
    stop("Could not find device type. Please specify the 'device' argument")
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
  meta <- utils::read.csv(file,
    sep = "=",
    header = FALSE,
    skip = meta_imin,
    nrows = cnames_imin - meta_imin - 3,
    row.names = 1,
    blank.lines.skip = FALSE
  )
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
  info <- replace(info, which(info == ""), NA)

  # import column names for main data structure
  cnames <- utils::read.csv(file,
    header = FALSE,
    skip = cnames_imin + 2,
    nrows = data_imin - cnames_imin - 4
  )$V3
  # remove last column due to encoding problems
  cnames <- cnames[-length(cnames)]

  # import the main data
  data <- utils::read.csv(file,
    header = FALSE,
    skip = data_imin,
    col.names = c("index", cnames, "fan")
  )

  # extract data with metabolic exchange parameters from data.
  # ZAN raw data files seem to be corrupt in some rare instances, containing
  # other kinds of data inside the data table. Breath-by-breath data points are
  # indicated by the prefix 'B' in the 'index' column.

  data <- data[grepl("B[0-9]", data$index), ]

  # -- TO DO --
  # import works only for files of German language

  # get load data
  if (any(colnames(data) == "Geschw.")) {
    # velocity is given in (m/min) in the raw data
    load_data <- round(data$Geschw. / 3600, 2)
  } else {
    load_data <- data$Last
  }

  # write a data frame for the main parameters
  df <- data.frame(
    time = data$Zeit / 1000, # time is given in milliseconds in the raw data
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    # length of inhalation plus exhalation in milliseconds gives respiratory
    # rate
    RR = 60000 / (data$tin + data$tex),
    # ventilation (per minute) is given in ml in the raw data
    VT = (data$Vin / 1000),
    VE = (60 * data$Vin) / (data$tin + data$tex), # calculate minute ventilation
    HR = data$HR,
    load = load_data,
    # incr = data$Steig./10, # variable currently not used
    PetO2 = as.numeric(NA),
    PetCO2 = as.numeric(NA)
  )

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  # sometimes values will be saved at the end of the raw data file (after the
  # last measurement). These will be removed.
  df <- df[1:which.max(df$time), ]

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro", "data.frame") # create spiro class
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
  if (grepl("\\.xls$", file) | grepl("\\.xlsx$", file)) { # Excel file
    # Read head of the Excel file
    head <- readxl::read_excel(file, range = "A1:B8", col_names = c("V1", "V2"))

    # files from Cosmed devices usually start with a line "ID-Code:"
    if (any(head == "ID code:", na.rm = TRUE) |
      any(head == "ID-Code:", na.rm = TRUE)) {
      device <- "cosmed"
      # files from Cortex devices usually contain a line at the head: "Bediener"
      # -- TO DO --
      # this is currently only working in German language
      # English equivalent is needed
    } else if (any(head == "Stammdaten", na.rm = TRUE)) {
      device <- "cortex"
    } else { # device type not found
      device <- "none"
    }
  } else if (grepl("\\.xml$", file)) { # xml file
    head <- import_xml(file, short = TRUE)
    if (any(head == "Stammdaten", na.rm = TRUE)) {
      device <- "cortex"
    } else {
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
    readxl::read_excel(file, range = "A1:B8", col_names = FALSE)
  )
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
  data <- readxl::read_excel(file, range = readxl::cell_cols(10:50))[-1:-2, ]
  # convert time to seconds (integer)
  l <- to_seconds(data$t)
  # remove rows with no time specified. These might occur at the end of the
  # file
  data <- data[l != 0, ]

  # -- TO DO --
  # rewrite search for speed and grade column
  # search for power column

  suppressWarnings(
    if (is.null(data$Speed)) {
      speed <- rep.int(0, length(l[l != 0]))
    } else {
      speed <- as.numeric(data$Speed)
    }
  )

  suppressWarnings(
    if (is.null(data$Grade)) {
      grade <- rep.int(0, length(l[l != 0]))
    } else {
      grade <- as.numeric(data$Grade)
    }
  )

  # write data
  df <- data.frame(
    time = l[l != 0], # exclude not specified times
    VO2 = as.numeric(data$VO2),
    VCO2 = as.numeric(data$VCO2),
    RR = as.numeric(data$Rf),
    VT = as.numeric(data$VT),
    VE = as.numeric(data$VE),
    HR = as.numeric(data$HR),
    load = round(speed / 36, 2),
    # incr = grade, # variable currently not used
    PetO2 = as.numeric(data$PetO2),
    PetCO2 = as.numeric(data$PetCO2)
  )

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  # rare special case if weight has been deleted from meta data. Recalculates
  # weight based on relative oxygen uptake present in raw data
  if (is.na(info$weight)) {
    info$weight <- round(
      # used first data value to recalculate weight
      as.numeric(data$VO2[[1]]) / as.numeric(data$`VO2/Kg`[[1]]), 1
    )
  }

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro", "data.frame") # create spiro class
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
  # read file
  if (grepl("\\.xml$", file)) { # xml file
    d <- import_xml(file)
  } else if (grepl("\\.xls$", file) | grepl("\\.xlsx$", file)) { # excel file
    d <- as.data.frame(
      suppressMessages(readxl::read_excel(file, col_names = FALSE))
    )
  } else {
    stop("Cortex raw data file must be in .xml, .xlsx or .xls format.")
  }

  # -- TO DO --
  # create import option for files of English language

  # get meta data (German language)

  # filter data frame for section with meta data
  meta_begin <- which(d == "Patient")
  meta_raw <- d[c(meta_begin:(meta_begin+25)),]

  # extract meta data
  name <- get_meta(meta_raw, c("Name", "Nachname"))
  surname <- get_meta(meta_raw, "Vorname")
  sex <- get_meta(meta_raw, "Geschlecht")
  birthday <- get_meta(meta_raw, "Geburtsdatum")
  height <- get_meta(meta_raw, "Gr\u00f6\u00dfe") # special handling for umlaute
  weight <- get_meta(meta_raw, "Gewicht")

  # write data frame for metadata
  info <- data.frame(name,
    surname,
    birthday,
    sex = get_sex(sex),
    height = to_number(height),
    weight = to_number(weight)
  )

  # get start of data section
  t_ind <- which(d[, 1] == "h:mm:ss")

  # get parameter labels
  cols <- as.character(d[t_ind-1, ])

  # get parameter count
  coln <- sum(!is.na(cols))

  # get raw data
  data <- d[(t_ind + 1):nrow(d), 1:coln]
  names(data) <- cols[1:coln]

  get_data <- function(data, vars) {
    col_matches <- colnames(data) %in% vars
    if (any(col_matches)) {
      # if more than one column matches with input, choose only first matched
      # column
      vars_match <- vars[min(which(vars %in% colnames(data)))]
      # Suppress warning if column content can not be converted to numbers
      # e.g. if empty cells are indicated by sign such as '-'
      out <- suppressWarnings(
        as.numeric(data[, vars_match])
      )
    } else {
      out <- NA
    }
    out
  }

  df <- data.frame(
    # use first column for time independent of name
    time = to_seconds(data[["t"]]),
    VO2 = get_data(data, c("V'O2 (STPD)", "V'O2")),
    VCO2 = get_data(data, "V'CO2"),
    RR = get_data(data, "AF"),
    VT = get_data(data, "VT"),
    VE = get_data(data, c("V'E (BTPS)", "V'E")),
    HR = get_data(data, "HF"),
    load = get_data(data, c("v", "P")),
    PetO2 = get_data(data, "PetO2"),
    PetCO2 = get_data(data, "PetCO2")
  )

  # in some cases VCO2 may be missing and thus is recalculated from RER and VO2
  if (all(is.na(df$VCO2))) {
    df$VCO2 <- df$VO2 / get_data(data, "RER")
  }

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro", "data.frame") # create spiro class
  df
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
    W = ,
    weiblich = ,
    female = "female",
    NA
  )
  if (is.null(sex)) sex <- NA
  out <- factor(sex, levels = c("female", "male", "diverse"))
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
  as.numeric(gsub("[^\\.0-9]", "", gsub(",", ".", chr)))
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
get_meta <- function(data, exprs) {
  meta_col <- data[which(data[, 1] %in% exprs), -1]
  if (any(!is.na(meta_col))) {
    out <- meta_col[[min(which(!is.na(meta_col)))]]
  } else {
    out <- NA
  }
  out
}

#' Import Excel XML to a R data frame
#'
#' \code{import_xml()} is a internal helper function to import a spreadsheet in
#' .xml format.
#'
#' @param file A character string, giving the path of a .xml file.
#' @param short Whether only the first rows of the table should be imported. Set
#'   this to true to extract features of the file within a fast computing time
#'   (e.g. when guessing the device type).
#'
#' @return A data.frame.
#' @noRd
import_xml <- function(file, short = FALSE) {
  # Get table rows
  rows <- xml2::xml_find_all(xml2::read_xml(file), "//d1:Table/d1:Row")

  # Get row data
  get_row_data <- function(row) {
    xml2::xml_text(xml2::xml_find_all(rows[row], ".//d1:Cell/d1:Data"))
  }
  # Get only data for first rows if short argument is chosen
  if (short) {
    n_max <- 10
  } else {
    n_max <- length(rows)
  }

  # -- TO DO --
  # this function call is quite slow and should be rewritten

  i <- lapply(seq_len(n_max), get_row_data)

  # find maximum column number
  col_n <- max(vapply(i, length, numeric(1)))

  # write data from xml table to data frame
  # empty columns are filled with NAs
  d <- as.data.frame(do.call(rbind, lapply(i, `[`, seq_len(col_n))))

  d
}
