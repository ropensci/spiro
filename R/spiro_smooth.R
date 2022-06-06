#' Apply a smoothing filter to data from cardiopulmonary exercise testing.
#'
#' Filter vectors and data frames with moving averages and digital filters.
#' Provides the data filtering for \code{\link{spiro_max}} and
#' \code{\link{spiro_plot}}.
#'
#' Raw data from cardiopulmonary is usually noisy due to measurement errors and
#' biological breath-to-breath variability. When processing or visualizing the
#' gas exchange data, it is often helpful to filter the raw data. This function
#' provides different filtering methods (time average, breath average, digital
#' filters).
#'
#' Breath-based and digital filters will be applied on the raw breath-by-breath
#' data. Time-based averages will be conducted on the interpolated data.
#'
#' @section Filtering Methods:
#' \describe{
#'   \item{Time-Based Average (e.g. \code{smooth = 30})}{A (centered) moving
#'     average over a defined timespan. The number can be given as an integer or
#'     as a character (e.g. \code{smooth = "30"}) and defines the length of the
#'     calculation interval in seconds.}
#'   \item{Breath-Based Average (e.g. \code{smooth = "15b"})}{A (centered)
#'     moving average over a defined number of breaths. The integer before the
#'     letter 'b' defines the number of breaths for the calculation interval.}
#'   \item{Butterworth filter (e.g. \code{smooth = "0.04f3"})}{A digital
#'     Butterworth filter (with lag). The number before the letter 'f' defines
#'     the low-pass cut-off frequency, the number after gives the order of the
#'     filter. See \code{\link{bw_filter}} for more details.}
#'   \item{Zero-lag Butterworth filter (e.g. \code{smooth = "0.04fz3"})}{A
#'     digital forwards-backwards Butterworth filter (without lag). The number
#'     before the letter 'f' defines the low-pass cut-off frequency, the number
#'     after gives the order of the filter. See \code{\link{bw_filter}} for more
#'     details.}
#' }
#'
#' @param data A data frame of the class \code{spiro}. Usually the output of
#'   the \code{\link{spiro} function}.
#' @param smooth An integer or character string specifying the smoothing
#'   strategy and parameters. Default is \code{30}, which means the applied
#'   filter is a 30-second moving average. See the section
#'   \strong{'Filtering Methods'} for more details.
#' @param columns A character vector of the data columns that should be
#'   filtered. By default the filtering applies to all data column of
#'   \code{data} (besides load, time and step).
#' @param quiet Whether warning message should be suppressed. Default is FALSE.
#'
#' @return A data frame
#' @export

spiro_smooth <- function(data, smooth = 30, columns = NULL, quiet = FALSE) {
  # check that data argument is a spiro data.frame
  if (!any(class(data) == "spiro")) {
    stop("'data' must be a spiro data frame (the output of a spiro() call)")
  }

  # get smoothing method
  s_method <- smooth_match(smooth)
  # get smoothing data
  s_data <- get_smooth_data(
    data = data,
    columns = columns,
    s_method = s_method,
    quiet = quiet
  )

  # vectorizes filter over all columns of the data frame
  out <- vapply(
    X = s_data,
    FUN = spiro_smooth.internal,
    FUN.VALUE = numeric(nrow(s_data)),
    method = s_method
  )
  out <- as.data.frame(out)

  # return smoothing method as attribute
  attr(out, "smooth_method") <- s_method

  out
}

#' Apply a smoothing filter to data a single numeric vector
#'
#' Internal function to \code{\link{spiro_smooth}}
#' @noRd
spiro_smooth.internal <- function(x, method) {
  out <- switch(method$type,
    breath = ,
    time = mavg(x, method$param),
    bw_zl = bw_filter(x, n = method$param$n, W = method$param$W),
    bw = bw_filter(x, n = method$param$n, W = method$param$W, zero_lag = FALSE)
  )
  out
}

#' Match the smooth argument to a filtering method
#'
#' Internal function for \code{\link{spiro_smooth}}
#' @noRd
smooth_match <- function(smooth) {
  if (grepl("f", smooth)) { # smooth method: Butterworth filter
    if (grepl("fz", smooth)) { # zero lag
      type <- "bw_zl"
      param <- bw_smooth_extract(smooth, "fz")
    } else { # with lag
      type <- "bw"
      param <- bw_smooth_extract(smooth, "f")
    }
    # Replace NAs with NULL values
    # When incorrect filter parameters are given, the filter functions will use
    # the default parameters instead
    param[which(is.na(param))] <- list(NULL)
  } else {
    if (grepl("b$", smooth)) {
      type <- "breath" # smooth method: breath averaging
      param <- gsub("b", "", smooth)
    } else {
      type <- "time" # smooth method: time averaging
      param <- smooth
    }
    # validate that time and breath smooth argument is appropriate
    param <- suppressWarnings(as.numeric(param))
    if (!is.numeric(param) || is.na(param)) {
      stop(
        paste0(
          "'smooth' argument is not valid. View ?spiro_smooth for valid smooth",
          " arguments"
        )
      )
    } else if (smooth < 1) {
      stop(
        "'smooth' must be greater or equal to 1 for time and breath averages"
      )
    }
  }

  out <- list(type = type, param = param)
  out
}

bw_smooth_extract <- function(smooth, matchstring = "f") {
  params <- strsplit(smooth, matchstring)[[1]]
  param_W <- suppressWarnings(as.numeric(params[1]))
  param_n <- suppressWarnings(as.numeric(params[2]))
  out <- list(W = param_W, n = param_n)
  out
}

#' Calculate the (centred) moving average
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
  # return NA vector if input is only NAs
  if (all(is.na(x))) {
    return(x)
  }
  # interpolate internal NAs to have the same NA handling as other filters
  x <- replace_intna(x)
  series <- stats::filter(x, rep(1 / k, k), sides = 2)
  as.vector(series)
}

#' Smooth data with a (zero-phase) Butterworth filter
#'
#' Internal function for \code{\link{spiro_smooth}}.
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
#' @param zero_lag Whether a zero phase (forwards-backwards) filter should be
#'   applied.
#'
#' @return A numeric vector of the same length as x.
bw_filter <- function(x, n = 3, W = 0.04, zero_lag = TRUE) {

  # return NA vector if input is only NAs
  if (all(is.na(x))) {
    return(x)
  }

  # Use default values when arguments are NULL (passed by other functions such
  # as spiro_smooth)
  if (is.null(n)) n <- 3
  if (is.null(W)) W <- 0.04

  # set filter
  bf <- signal::butter(n, W, "low")

  # handle of internal NAs
  # internal NAs can not be processed by Butterworth filters. For the zero-phase
  # method used in this package, leading and trailing NAs can also not be
  # processed. To overcome these issues all internal NAs will be linearly
  # interpolated

  if (zero_lag) {
    # the signal package currently only contains an old version of the zero-lag
    # filter function `filtfilt()`, which does not minimize end-transients. To
    # overcome this issue this function pads the signal in reverse order before
    # the beginning and at the end of the series. This is equal to the
    # even padtype in Python's scipy.signal.filtfilt()
    x_ext <- replace_intna(c(rev(x), x, rev(x)))
    # Remaining leading or trailing NAs should be now irrelevant to the filter
    # procedure, but need to be replaces to make signal::filtfilt() work
    x_ext[which(is.na(x_ext))] <- 0
    out_pre <- signal::filtfilt(bf, x_ext)
    out <- out_pre[(length(x) + 1):(2 * length(x))]
  } else {
    out <- signal::filter(bf, replace_intna(x))
    # in some instances the filter length varies (this is currently not
    # predictable for me). To prevent errors, missing vector entries will be
    # filled with NAs
    if (length(out) < length(x)) {
      out <- c(out, rep.int(NA, length(x) - length(out)))
    }
  }
  as.vector(out)
}

#' Replace internal NAs with interpolated values
#'
#' Internal function for \code{\link{bw_filter}} in \code{\link{spiro_smooth}}
#' @noRd
replace_intna <- function(data) {
  out <- stats::approx(x = seq_along(data), y = data, xout = seq_along(data))
  out$y
}

#' Get the right data for smoothing
#'
#' Internal function for \code{\link{spiro_smooth}}.
#' @noRd
get_smooth_data <- function(data, columns, s_method, quiet = FALSE) {
  if (!is.logical(quiet)) {
    stop("'quiet' must be either TRUE or FALSE")
  }

  # breath based and digital filter methods are per default applied on the raw
  # breath-by-breath data
  if (s_method$type != "time") {
    # get raw data
    rawdata <- attr(data, "raw")
    if (check_bb(rawdata$time)) { # raw data is breath-by-breath
      if (is.null(columns)) {
        # use all columns (besides time and load) if columns argument is empty
        columns <- names(rawdata)[!names(rawdata) %in% c("time", "load")]
      }
      if (all(columns %in% names(rawdata))) {
        data <- rawdata[columns]
      } else {
        # default to interpolated data if column names are present in it, but
        # not in raw data
        if (all(columns %in% names(data))) {
          data <- data[columns]
          w <- get_wrongcol_string(data = rawdata, columns = columns)
          if (!quiet) {
            warning(
              paste0(
                "Could not find column name(s) ",
                w,
                " in raw data. Uses interpolated data for smoothing instead."
              ),
              call. = FALSE
            )
          }
        } else {
          w <- get_wrongcol_string(data = data, columns = columns)
          stop(
            paste0(
              "Could not find columns name(s) ",
              w,
              " in raw or interpolated data"
            ),
            call. = FALSE
          )
        }
      }
    } else { # raw data is not breath-by-breath
      if (!quiet) {
        warning(
          "Raw data is not breath-by-breath. Uses interpolated data instead.",
          call. = FALSE
        )
      }
      if (is.null(columns)) {
        columns <- names(data)[!names(data) %in% c("time", "load", "step")]
      }
      if (all(columns %in% names(data))) {
        data <- data[columns]
      } else {
        w <- get_wrongcol_string(data = data, columns = columns)
        stop(
          paste0("Could not find columns name(s) ", w, " in interpolated data"),
          call. = FALSE
        )
      }
    }
  } else {
    # use all columns (besides time,load and step) if columns argument is empty
    if (is.null(columns)) {
      columns <- names(data)[!names(data) %in% c("time", "load", "step")]
    }
    if (all(columns %in% names(data))) {
      data <- data[columns]
    } else {
      # wrong column names(s)
      w <- get_wrongcol_string(data = data, columns = columns)
      stop(
        paste0("Could not find columns name(s) ", w, " in data"),
        call. = FALSE
      )
    }
  }
  data
}

#' Get a string for wrong column names
#'
#' Internal function for messages and errors in \code{\link{spiro_smooth}}.
#' @noRd
get_wrongcol_string <- function(data, columns) {
  # determine wrong column names
  w <- columns[!columns %in% names(data)]
  out <- paste0("'", w, "'", collapse = ", ")
  out
}
