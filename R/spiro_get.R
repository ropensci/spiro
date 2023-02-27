#' Import raw data from spiroergometric devices (deprecated)
#'
#' This function has been deprecated as of package version \code{0.2.0}. It will
#' be removed in the next version release. Please use \code{\link{spiro}} for
#' automated import and processing or \code{\link{spiro_raw}} to import only raw
#' data.
#'
#' @inheritParams spiro
#' @export

spiro_import <- function(file, device = NULL, anonymize = TRUE) {
  .Deprecated(
    new = "spiro_raw",
    msg =
      paste0(
        "'spiro_import' is deprecated and will be removed in the next package ",
        "release. Use 'spiro' for automated import and processing or ",
        "'spiro_raw' to import only raw data."
      )
  )
  spiro_raw(data = file, device = device, anonymize = anonymize)
}

#' Import raw data from spiroergometric devices
#'
#' Internal function to import raw data from metabolic cart files
#'
#' @noRd

spiro_get <- function(file, device = NULL, anonymize = TRUE) {
  if (is.null(device)) device <- guess_device(file)
  out <- switch(device,
    zan = spiro_get_zan(file),
    cosmed = spiro_get_cosmed(file),
    cortex = spiro_get_cortex(file),
    vyntus = spiro_get_vyntus(file),
    stop("Could not find device type. Please specify the 'device' argument")
  )
  if (anonymize) {
    attr(out, "info") <- spiro_anonymize(attr(out, "info"))
  }

  # Remove null values caused by measurement errors
  out$VO2[which(out$VO2 == 0)] <- NA
  out$VCO2[which(out$VCO2 == 0)] <- NA
  # Remove rows with empty time data
  out <- out[which(!is.na(out$time)), ]

  out
}

#' Import raw data from ZAN spiroergometric devices
#'
#' \code{spiro_get_zan()} retrieves cardiopulmonary data from ZAN
#' metabolic cart files.
#'
#' @noRd
spiro_get_zan <- function(file) {
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
    bodymass = as.numeric(meta_df$gewicht)
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
#' devices visit \code{\link{spiro}}.
#'
#' @noRd
guess_device <- function(file) {
  if (grepl("\\.xls$", file, ignore.case = TRUE) ||
    grepl("\\.xlsx$", file, ignore.case = TRUE)) { # Excel file
    # Read head of the Excel file
    head <- readxl::read_excel(file, range = "A1:B8", col_names = c("V1", "V2"))

    # files from Cortex devices usually contain a line at the head:
    # "Administrative Data" or "Stammdaten"
    if (any(
      head == "Administrative Data" | head == "Stammdaten",
      na.rm = TRUE
    )
    ) {
      device <- "cortex"
      # files from Cosmed devices usually start with a line "ID-Code:" or "ID"
    } else if (any(grepl("ID", head))) {
      device <- "cosmed"
    } else { # device type not found
      device <- "none"
    }
  } else if (grepl("\\.xml$", file, ignore.case = TRUE)) { # xml file
    head <- import_xml(file, short = TRUE)
    if (any(
      head == "Administrative Data" | head == "Stammdaten",
      na.rm = TRUE
    )
    ) {
      device <- "cortex"
    } else {
      device <- "none"
    }
  } else { # non-Excel file
    # read the first rows of the file
    head <- utils::read.delim(file, header = FALSE, nrows = 5)
    # remove leading or trailing white spaces that may occur in some Vyntus
    # files
    head <- apply(head, 2, trimws)
    # files from ZAN devices usually start with a line "[person]"
    if (any(head == "[person]")) {
      device <- "zan"
    } else if (any(head == "Tid" | head == "Temps" | head == "Zeit")) {
      device <- "vyntus"
    } else {
      device <- "none"
    }
  }
  device
}

#' Import raw data from COSMED spiroergometric devices
#'
#' \code{spiro_get_cosmed()} retrieves cardiopulmonary data from ZAN
#' metabolic cart files.
#'
#' @noRd
spiro_get_cosmed <- function(file) {
  # read meta data
  tbl <- suppressMessages(
    readxl::read_excel(file, range = "A1:B8", col_names = FALSE)
  )
  # write as data frame
  tbl <- as.data.frame(tbl)

  # find meta data

  info <- data.frame(
    name = get_meta(tbl, c("First name:", "First name", "Vorname:", "Vorname")),
    surname = get_meta(
      tbl,
      c("Last name:", "Last name", "Nachname:", "Nachname")
    ),
    birthday = NA, # cosmed files do only display an age, but no birthday column
    sex = get_sex(get_meta(tbl, c("Sex:", "Sex", "Geschlecht:", "Geschlecht"))),
    height = as.numeric(
      get_meta(
        tbl,
        c(
          "Height (cm):", "Height (cm)",
          "Gr\u00f6\u00dfe (cm):", "Gr\u00f6\u00dfe (cm)"
        )
      )
    ),
    bodymass = as.numeric(
      get_meta(
        tbl,
        c("Weight (Kg):", "Weight (Kg)", "Gewicht (Kg):", "Gewicht (Kg)")
      )
    )
  )

  # read main data
  data <- as.data.frame(
    readxl::read_excel(file, range = readxl::cell_cols(10:50))
  )

  # Get unit data and remove rows with data units
  unit_data <- data[1, ]
  data <- data[-1:-2, ]

  if (any(colnames(data) == "Speed")) {
    sp_unit <- unit_data[["Speed"]]
    load_data <- get_data(data, "Speed")
    if (sp_unit == "Kmh*10") {
      load_data <- load_data / 10
    }
  } else { # no velocity data available
    load_data <- get_data(data, "Power")
  }

  # Check if time data import worked
  # In some cases the time data may not be correctly imported, due to the
  # cell-formatting of the time column in Excel.
  if (!grepl("[0-9][0-9]\\:[0-9][0-9]", data$t[1])) {
    t <- as.data.frame(
      suppressWarnings(
        readxl::read_excel(file,
          range = readxl::cell_cols(10),
          col_types = "date"
        )[-c(1, 2), ]
      )
    )
    data["t"] <- format(as.vector(t)[[1]], format = "%H:%M:%S")
  }

  # write data
  df <- data.frame(
    time = to_seconds(data[["t"]]),
    VO2 = get_data(data, "VO2"),
    VCO2 = get_data(data, "VCO2"),
    RR = get_data(data, c("Rf", "Af")),
    VT = get_data(data, "VT"),
    VE = get_data(data, "VE"),
    HR = get_data(data, c("HR", "HF")),
    load = load_data,
    PetO2 = get_data(data, c("PetO2", "PeO2")),
    PetCO2 = get_data(data, c("PetCO2", "PeCO2"))
  )

  # remove rows with no time specified. These might occur at the end of the
  # file
  df <- df[df$time != 0, ]

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  # rare special case if body mass has been deleted from meta data. Recalculates
  # body mass based on relative oxygen uptake present in raw data
  if (is.na(info$bodymass)) {
    info$bodymass <- round(
      # used first data value to recalculate body mass
      as.numeric(data$VO2[[1]]) / as.numeric(data$`VO2/Kg`[[1]]), 1
    )
  }

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro", "data.frame") # create spiro class
  df
}

#' Import raw data from Cortex spiroergometric devices
#'
#' \code{spiro_get_cortex()} retrieves cardiopulmonary data from cortex
#' metabolic cart files.
#'
#' @noRd
spiro_get_cortex <- function(file) {
  # read file
  if (grepl("\\.xml$", file, ignore.case = TRUE)) { # xml file
    d <- import_xml(file)
  } else if (grepl("\\.xls$", file, ignore.case = TRUE) ||
    grepl("\\.xlsx$", file, ignore.case = TRUE)) { # excel file
    d <- as.data.frame(
      suppressMessages(readxl::read_excel(file, col_names = FALSE))
    )
  } else {
    stop("Cortex raw data file must be in .xml, .xlsx or .xls format.")
  }

  # get meta data

  # filter data frame for section with meta data
  meta_begin <- which(d == "Patient data" | d == "Patient")
  meta_raw <- d[c(meta_begin:(meta_begin + 25)), ]

  # extract meta data
  name <- get_meta(meta_raw, c("Last Name", "Name", "Nachname"))
  surname <- get_meta(meta_raw, c("First Name", "Vorname"))
  sex <- get_meta(meta_raw, c("Sex", "Geschlecht"))
  birthday <- get_meta(meta_raw, c("Date of Birth", "Geburtsdatum"))
  # special handling for umlaute in German files
  height <- get_meta(meta_raw, c("Height", "Gr\u00f6\u00dfe"))
  bodymass <- get_meta(meta_raw, c("Weight", "Gewicht"))

  # write data frame for metadata
  info <- data.frame(name,
    surname,
    birthday,
    sex = get_sex(sex),
    height = to_number(height),
    bodymass = to_number(bodymass)
  )

  # get start of data section
  t_ind <- which(
    d[, 1] == "h:mm:ss" | d[, 1] == "h:mm:ss,ms" | d[, 1] == "h:mm:ss.ms"
  )

  # get parameter labels
  cols <- as.character(d[t_ind - 1, ])

  # get parameter count
  coln <- sum(!is.na(cols))

  # get raw data
  data <- d[(t_ind + 1):nrow(d), 1:coln]
  names(data) <- cols[1:coln]

  df <- data.frame(
    time = to_seconds(data[["t"]]),
    VO2 = get_data(data, c("V'O2 (STPD)", "V'O2")),
    VCO2 = get_data(data, "V'CO2"),
    RR = get_data(data, c("AF", "BF")),
    VT = get_data(data, "VT"),
    VE = get_data(data, c("V'E (BTPS)", "V'E")),
    HR = get_data(data, c("HR", "HF")),
    load = get_data(data, c("v", "P", "WR")),
    PetO2 = get_data(data, "PetO2"),
    PetCO2 = get_data(data, "PetCO2")
  )

  # in some cases VCO2 may be missing and thus is recalculated from RER and VO2
  if (all(is.na(df$VCO2))) {
    df$VCO2 <- df$VO2 / get_data(data, "RER")
  }

  # correct units
  # VO2 and VCO2 might sometimes be given in l/min instead of ml/min
  if (max(df$VO2, na.rm = TRUE) < 100) df$VO2 <- df$VO2 * 1000
  if (max(df$VCO2, na.rm = TRUE) < 100) df$VCO2 <- df$VCO2 * 1000

  # Write null values in HR as NAs
  df$HR[which(df$HR == 0)] <- NA

  attr(df, "info") <- info # write meta data
  class(df) <- c("spiro", "data.frame") # create spiro class
  df
}

#' Import raw data from Vyntus spiroergometric devices
#'
#' \code{spiro_get_vyntus()} retrieves cardiopulmonary data from Vyntus
#' metabolic cart files.
#'
#' @noRd
spiro_get_vyntus <- function(file) {
  # get head of file to find column names and start of data
  head <- utils::read.delim(file, header = FALSE, nrows = 10)

  # sometimes the files will contain leading or trailing whitespaces
  # complicating the character matching. These are removed first.
  head_rm <- as.data.frame(apply(head, 2, trimws))

  colstart <- which(
    head_rm == "Tid" | head_rm == "Temps" | head_rm == "Zeit",
    arr.ind = TRUE
  )

  data <- utils::read.delim(file, skip = colstart[[1]] - 1)[-1, ]
  # remove whitespaces
  data <- as.data.frame(apply(data, 2, trimws))
  # remove first row if it containes empty values
  if (data[1, 1] == "") data <- data[-1, ]
  # convert all but the time column to numbers (i.e replace comma as decimal
  # mark if necessary and set missing values (`-`) to NA)
  data_mod <- as.data.frame(cbind(data[, 1], apply(data[, -1], 2, to_number)))
  colnames(data_mod)[1] <- colnames(data)[1]

  df <- data.frame(
    time = to_seconds(
      get_data(data_mod, c("Tid", "Temps", "Zeit"), as_numeric = FALSE)
    ),
    VO2 = get_data(data_mod, "V.O2"),
    VCO2 = get_data(data_mod, "V.CO2"),
    RR = get_data(data_mod, c("BF", "FR")),
    VT = get_data(data_mod, "VTex"),
    VE = get_data(data_mod, c("V.E", "VeSTPD")),
    HR = get_data(data_mod, c("HF", "FC")),
    load = get_data(data_mod, c("Last", "Vitesse", "Watt")),
    PetO2 = get_data(data_mod, "PETO2") * 7.50062, # convert from kPa to mmHg
    PetCO2 = get_data(data_mod, "PETCO2") * 7.50062 # convert from kPa to mmHg
  )

  # use power data if velocity data is empty
  if (all(df$load == 0, na.rm = TRUE)) df$load <- get_data(data_mod, c("Watt"))

  # Recalculate body mass from relative oxygen uptake data (if given)
  if (any(colnames(data_mod) == "V.O2.kg" | colnames(data_mod) == "VO2.kg")) {
    bodymass <- round(
      mean(df$VO2 / get_data(data_mod, c("V.O2.kg", "VO2.kg")), na.rm = TRUE),
      1
    )
  } else {
    bodymass <- NA
  }

  # Recalculate VE from EqO2 if necessary and possible
  if (all(is.na(df$VE)) & any(colnames(data_mod) == "EqO2")) {
    df$VE <- get_data(data_mod, "EqO2") * df$VO2 / 1000
  }

  # Write meta data
  info <- data.frame(
    name = NA,
    surname = NA,
    birthday = NA,
    sex = NA,
    height = NA,
    bodymass = bodymass
  )

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
  if (grepl("nnlich", chr)) { # handling to avoid umlaut in mannlich
    sex <- "male"
  } else {
    sex <- switch(chr,
      m = ,
      "M" = ,
      male = "male",
      f = ,
      "F" = ,
      w = ,
      W = ,
      weiblich = ,
      Weiblich = ,
      female = "female",
      NA
    )
  }
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

  # Get only data for first rows if short argument is chosen
  if (short) {
    rows <- rows[1:10]
  }

  i <- lapply(lapply(rows, xml2::xml_children), xml2::xml_text)

  # find maximum column number
  col_n <- max(vapply(i, length, numeric(1)))

  # write data from xml table to data frame
  # empty columns are filled with NAs
  d <- as.data.frame(do.call(rbind, lapply(i, `[`, seq_len(col_n))))

  d
}

#' Get data from a data frame by name
#'
#' \code{get_data()} is a helper function to retrieve data from a matching
#' column of a data.frame
#'
#' @param data A data.frame containing the data
#' @param vars A character vector. The function tries to match its values with
#'   the column names of the data.frame in the given order.
#'
#' @return A numeric vector. Any characters or empty fields will be coerced to
#'   NAs.
#' @noRd
get_data <- function(data, vars, as_numeric = TRUE) {
  col_matches <- colnames(data) %in% vars
  if (any(col_matches)) {
    # if more than one column matches with input, choose only first matched
    # column
    vars_match <- vars[min(which(vars %in% colnames(data)))]
    # Suppress warning if column content can not be converted to numbers
    # e.g. if empty cells are indicated by sign such as '-'
    out <- suppressWarnings(
      if (as_numeric) as.numeric(data[, vars_match]) else data[, vars_match]
    )
  } else {
    out <- if (as_numeric) as.numeric(NA) else NA
  }
  out
}

#' Anonymize participants meta data
#'
#' \code{spiro_anonymize()} replaces personal information from meta data
#' imported by an id.
#'
#' @param info A data.frame containing meta data, as produced as an attribute by
#'   the spiro_get_* functions.
#'
#' @return A data.frame containing the id and the body mass.
#' @noRd
spiro_anonymize <- function(info) {
  # consider birthday data only if available
  if (is.na(info$birthday)) {
    birthday <- NULL
  } else {
    birthday <- info$birthday
  }

  # replace personal information by anonymized id
  id <- get_anonid(
    name = info$name,
    surname = info$surname,
    birthday = birthday
  )

  # drop all personal information despite body mass data
  out <- data.frame(
    id = id,
    bodymass = info$bodymass
  )
  out
}


#' Get the anonymization id from personal data
#'
#' \code{get_anonid()} returns the anonymization id corresponding to given
#' personal data.
#'
#' By default, the spiro package anonymizes personal information obtained from
#' file meta data. The data are saved to the "info" attribute of a spiro() call.
#' The default anonymization ensures that no personal information is
#' accidentally revealed, e.g. by sharing spiro outputs as .Rda files.
#'
#' While there is no way to directly deanonymize the data, get_anonid() allows
#' you to recreate the ids, when meta data (name, surname and birthday) are
#' known. Birthday is only used within the id generation if available in the
#' original raw data.
#'
#' To disable the anonymization process during import use spiro(anonymize =
#' FALSE)
#'
#' @param name A character string, containing the participant's name as present
#'   in the raw data file.
#' @param surname A character string, containing the participant's surname as
#'   present in the raw data file.
#' @param birthday A character string, containing the participant's birthday as
#'   present in the raw data file. If no birthday data is available in the raw
#'   data, this is ignored.
#'
#' @return A character string, containing the anonymized id.
#'
#' @examples
#' get_anonid("Jesse", "Owens", "12.09.1913")
#' @export
get_anonid <- function(name, surname, birthday = NULL) {
  digest::digest(c(name, surname, birthday), "crc32")
}
