

spiro_import <- function(file, type = NULL) {
  file <- get_path(file)
  if (is.null(type)) type <- guess_type(file)
  switch(type,
         zan = spiro_import_zan(file),
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
  "zan"
}
