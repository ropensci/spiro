#' Handle time duplicates before interpolation or plotting
#'
#' Removes duplicates from a vector, by replacing them by nearby values.
#' Internal function to \code{\link{spiro_interpolate}} and
#' \code{\link{hr_interpolate}}. Also used for \code{\link{spiro_plot}} when the
#' `smooth` argument works with the raw data.
#'
#' Duplicates will usually only occur when data is measured breath-by-breath (or
#' in irregular time intervals for heart rate data) and is rounded to seconds.
#' In such cases it may happen that data rows end to have the same time stamp.
#' This causes problems in interpolation or reshaping for plotting.
#'
#' @param values Numeric vector
#' @return A numeric vector with replaced duplicated values
#' @noRd
dupl <- function(values) {
  d <- anyDuplicated(values)
  if (d != 0) {
    # find each duplicate
    ds <- unique(values[duplicated(values)])
    for (i in ds) {
      n <- sum(values == i) # number of same duplicate
      # calculate replacement values
      val_pre <- seq(-0.5, 0.5, length.out = n + 2)
      val_repl <- val_pre[-c(1, length(val_pre))]
      # replace duplicated values
      values[values == i] <- values[values == i] + val_repl
    }
  }
  values
}

#' Convert time data to seconds
#'
#' \code{to_seconds()} converts time data of the form hh:mm:ss to seconds.
#'
#' @param time_data A character vector containing the time data in the format
#'   hh:mm:ss or mm:ss.
#' @noRd
to_seconds <- function(time_data) {
  vapply(
    time_data,
    to_seconds.internal,
    FUN.VALUE = numeric(1),
    USE.NAMES = FALSE
  )
}

#' Convert single time variable to seconds
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
  time <- gsub(",", ".", time)
  # split time by double colon separator
  split <- strsplit(time, ":")[[1]]
  time_split <- as.numeric(split) # convert vectors to numbers

  # calculate time in seconds based on input format
  if (length(time_split) == 3) {
    # for hh:mm:ss or hh:mm:ss.s
    s <- 3600 * time_split[[1]] + 60 * time_split[[2]] + time_split[[3]]
  } else if (length(time_split) == 2) {
    # for mm:ss
    s <- 60 * time_split[[1]] + time_split[[2]]
  }
  s
}

#' Check if data is breath-by-breath
#'
#' Internal function to \code{?link{spiro_interpolate}}
#'
#' Data is considered as non-breath-by-breath if the mean interval between two
#' data points exceeds five seconds.
#'
#' @param timedata Numeric vectors giving the time data (in seconds)
#' @return A logical vector, indicating if the data is breath-by-breath data
#' @noRd
check_bb <- function(timedata) {
  m <- mean(diff(timedata))
  if (m < 5) {
    out <- TRUE
  } else {
    out <- FALSE
  }
  out
}
