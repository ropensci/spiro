#' Handle time duplicates before interpolation
#'
#' Internal function to \code{\link{spiro_interpolate}} and
#' \code{\link{hr_interpolate}}
#'
#' To ensure that any time duplicates in a dataset will still be correctly
#' interpolated, the existing duplicates are slightly separated.
#'
#' @param values Numeric vector to check for duplicates
#' @noRd
dupl <- function(values) {
  d <- anyDuplicated(values)
  if (d != 0) {
    ds <- which(duplicated(values))
    for (di in ds) {
      values[[di - 1]] <- (values[[di - 1]] - 0.1)
      values[[di]] <- (values[[di]] + 0.1)
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

#' Create a (centred) moving average
#'
#' Internal function for \code{\link{spiro_max}} and \code{\link{spiro_plot}}
#'
#' @param x A numeric vector on which the moving average should be applied
#' @param k Length of the interval for the rolling average
#'
#' @return A numeric vector of the same length as x. Leading and trailing
#'   entries are filled with NAs.
#' @noRd
mavg <- function(x, k) {
  series <- stats::filter(x, rep(1 / k, k), sides = 2)
  as.vector(series)
}

#' Smooth data with a (zero-lag) Butterworth filter
#'
#' Internal function for \code{\link{spiro_max}} and \code{\link{spiro_plot}}
#'
#' Digital filtering might be a preferable processing strategy for smoothing
#' data from gas exchange measures when compared to moving averages. Robergs et
#' al. (2010) proposes a third order Butterworth filter with a low-pass cut-off
#' frequency of 0.04 for filtering VO2 data.
#'
#' It should be noted that Butterworth filter comprise a time lag. A method to
#' create a data series with zero lag is to subsequently apply two Butterworth
#' filters in forward and reverse direction (forward-backwards filtering). While
#' this procedure removes any time lag it changes the magnitude of the filtering
#' response, i.e. the resulting filter has not the same properties (order and
#' cut-off frequency) as a single filter has.
#'
#' @param x A numeric vector on which the digital filter should be applied
#' @param n Order of the Butterworth filter, defaults to 3
#' @param W Low-pass cut-off frequency of the filter, defaults to 0.04
#' @param zero_lag Whether a zero lag (forwards-backwards) filter should be
#'   applied.
#'
#' @return A numeric vector of the same length as x.
#' @noRd
bw_filter <- function(x, n = 3, W = 0.04, zero_lag = TRUE) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop(paste0(
      "For digital filtering, the package 'signal' must be installed. ",
      "Run 'install.packages('signal')' in your console."),
      call. = FALSE
    )
  }

  # set filter
  bf <- signal::butter(n, W, "low")

  # handle of internal NAs
  # internal NAs can not be processed by Butterworth filters. For the zero-lag
  # method used in this package, leading and trailing NAs can also not be
  # processed. To overcome these issues all internal NAs will be linearly
  # interpolated

  if (zero_lag) {
    # the signal package currently only contains an old version of the zero-lag
    # filter function `filtfilt()`, which does not minimize end-transients. To
    # overcome this issue this function pads the signal in reverse order before
    # the beginning and at the end of the series.
    x_ext <- replace_intna(c(rev(x), x, rev(x)))
    out_pre <- signal::filtfilt(bf, x_ext)
    out <- out_pre[(length(x) + 1):(2 * length(x))]
  } else {
    out <- signal::filter(bf, replace_intna(x))
  }
  as.vector(out)
}

replace_intna <- function(data) {
  out <- stats::approx(x = seq_along(data), y = data, xout = seq_along(data))
  out$y
}

