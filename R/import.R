

spiro_import <- function(file, device = NULL) {
  file <- get_path(file)
  if (is.null(device)) device <- guess_type(file)
  switch(device,
         zan = spiro_import_zan(file),
         cosmed = spiro_import_cosmed(file),
         stop("'type' not specified")
         )
}

spiro_import_zan <- function(file) {
  ldf <- utils::read.csv(file, nrows = 7, skip = 1, sep = "=", header = FALSE, row.names = 1)
  ldf <- data.frame(t(ldf))
  info <- data.frame(
    name = ldf$vorname,
    surname = ldf$name,
    birthday = ldf$geburtstag,
    sex = ldf$geschlecht,
    height = as.numeric(ldf$groesse),
    weight = as.numeric(ldf$gewicht)
  )

  cnames <- c("index",utils::read.csv(file, skip = 13, nrows = 96, header = FALSE)$V3)
  data <- utils::read.csv(file, skip = 111, header = FALSE, col.names = cnames)
  df <- data.frame(
    time = data$Zeit/1000,
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    HR = data$HR,
    velocity = round(data$Geschw./3600,2),
    incr = data$Steig./10
  )
  attr(df, "info") <- info
  df
}

get_path <- function(name) {
  filepath <- list.files(path = "..", pattern = name, recursive = TRUE, full.names = TRUE)
  if (length(filepath) != 1)  filepath <- name
  filepath
}

guess_type <- function(file) {
  if (grepl("\\.xls", file)) device <- "cosmed"
  else device <- "zan"
}

spiro_import_cosmed <- function(file) {
  tbl <- suppressMessages(readxl::read_excel(file, range = "A1:B8", col_names = FALSE))
  ldf <- data.frame(t(as.data.frame(tbl[[2]], row.names = tbl[[1]])))

  if (tbl[[1]][[2]] == "Nachname:") { # German language
    name <- "Vorname."
    surname <- "Nachname."
    age <- "Alter."
    sex <- "Geschlecht."
    height <- "Gr\u00f6\u00dfe..cm.."
    weight <- "Gewicht..Kg.."
  } else { # English language
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
    sex = ldf[[sex]],
    height = as.numeric(ldf[[height]]),
    weight = as.numeric(ldf[[weight]])
  )

  data <- readxl::read_excel(file, range = readxl::cell_cols(10:50))[-1:-2,]
  l <- to_seconds(data$t)
  data <- data[l != 0,]

  suppressWarnings(
    if (is.null(data$Speed)) speed <- rep.int(0,length(l[l != 0]))
    else speed <- as.numeric(data$Speed)
  )
  suppressWarnings(
    if (is.null(data$Grade)) grade <- rep.int(0,length(l[l != 0]))
    else grade <- as.numeric(data$Grade)
  )
  df <- data.frame(
    time = l[l != 0],
    VO2 = data$VO2,
    VCO2 = data$VCO2,
    HR = data$HR,
    velocity = round(speed/36,2),
    incr = grade
  )
  if (is.na(info$weight)) info$weight <- round(as.numeric(data$VO2[[1]]) / as.numeric(data$`VO2/Kg`[[1]]),1)

  attr(df, "info") <- info
  df
}


to_seconds <- function(time_data) {
  sapply(time_data, to_seconds.internal, USE.NAMES = FALSE)
}

to_seconds.internal <- function(time) {
  time_split <- as.numeric(strsplit(time,":")[[1]])
  if (length(time_split == 3)) {
    s <- 3600*time_split[[1]]+60*time_split[[2]]+time_split[[3]]
  } else if (length(time_split == 2)) {
    s <- 60*time_split[[1]]+time_split[[2]]
  }
  s
}
