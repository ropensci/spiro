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
#' The function is vectorized. The input can be either a numeric vectors or a
#' data frame. Its functionality is the basis for the evaluation of the
#' \code{smooth} argument in \code{\link{spiro_max}} and
#' \code{\link{spiro_plot}}.
#'
#' @section Filtering Methods:
#' \describe{
#'   \item{Time-Based Average (e.g. \code{smooth = 30})}{A (centered) moving
#'     average over a defined timespan. The number can be given as an integer or
#'     as a character (e.g. \code{smooth = "30"}) and defines the length of the
#'     calculation interval in seconds.}
#'   \item{Breath-Based Average (e.g. \code{smooth = "15b"})}{A (centered)
#'     moving average over a defined number of breaths. The integer before the
#'     letter 'b' defines the number of breaths for the calculation interval.
#'     Note that when using a breath-based average, an object of class
#'     \code{spiro} containing the raw breath data as the \code{raw} attribute
#'     has to be set as the \code{rawsource} argument.}
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
#' @param data A numeric vector or data frame with numeric vectors as columns.
#'   Usually (a subset of) the output of \code{\link{spiro}}.
#' @param smooth An integer or character string specifying the smoothing
#'   strategy and parameters. Default is \code{30}, which means the applied
#'   filter is a 30-second moving average. See the section
#'   \strong{'Filtering Methods'} for more details.
#' @param rawsource A data frame of the class \code{spiro} (as given by the
#'   \code{\link{spiro}} function) as additional data source for raw breath
#'   data, when this is not included in the \code{data} argument. This argument
#'   is only needed when a breath-based moving average is used as filtering
#'   method.
#'
#' @return A numeric vector (if input was a vector) or a data frame (if input
#'   was a data frame or a list of vectors) of the same length as the input
#' @export

spiro_smooth <- function(data, smooth = 30, rawsource = NULL) {
  # get smoothing method
  s_method <- smooth_match(smooth)

  # if breathe averaging is choosing as the smoothing method, the required data
  # (raw breath data) is usually not in the spiro class data frame but in its
  # attributes. Thus when when using a subset of the spiro data frame as the
  # 'data' argument in spiro_smooth() the information is missing.
  # To be able to calculate breath averages the orginal spiro data frame has to
  # be given as the additional rawsource argument. The functions matches the raw
  # data to the given 'data' argument, which requires named vectors/data frames.
  # If another averaging method is used, the rawsource argument will be ignored.
  # Breath averaging on data without the spiro class will behave similar to time
  # averaging (i.e. treating every vector entry as one second/breath).
  if (s_method$type == "breath") {
    if (any(class(data) == "spiro")) {
      if (!any(class(rawsource) == "spiro")) {
        stop(
          paste0(
            "When using breath averages, give a spiro data frame as the ",
            "'rawsource' arguments to spiro_smooth()"
          ),
          call. = FALSE
        )
      }
      data <- attr(rawsource, "raw")[names(data)]
    } else {
      warning(
        paste0(
          "When applied to an object not of class 'spiro' ",
          "breath-averaging will return the same results as time averaging"
        ),
        call. = FALSE
      )
    }
  }

  # spiro_smooth is a vectorized function. It works for either single vectors or
  # data.frames, where the columns are numeric vectors.
  if (any(class(data) == "data.frame")) {
    out <- vapply(
      X = data,
      FUN = spiro_smooth.internal,
      FUN.VALUE = numeric(nrow(data)),
      method = s_method
    )
    out <- as.data.frame(out)
  } else {
    out <- spiro_smooth.internal(x = data, method = s_method)
  }

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
      stop("'smooth' must be greater or equal to 1 for time and breath averages")
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
  # interpolate internal NAs to have the same NA handling as other filters
  x <- replace_intna(x, keep_NA_all = TRUE)
  series <- stats::filter(x, rep(1 / k, k), sides = 2)
  as.vector(series)
}

#' Smooth data with a (zero-lag) Butterworth filter
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
#' @param zero_lag Whether a zero lag (forwards-backwards) filter should be
#'   applied.
#'
#' @return A numeric vector of the same length as x.
bw_filter <- function(x, n = 3, W = 0.04, zero_lag = TRUE) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop(paste0(
      "For digital filtering, the package 'signal' must be installed. ",
      "Run 'install.packages('signal')' in your console."
    ),
    call. = FALSE
    )
  }

  # Use default values when arguments are NULL (passed by other functions such
  # as spiro_smooth)
  if (is.null(n)) n <- 3
  if (is.null(W)) W <- 0.04

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
    # Remaining leading or trailing NAs should be now irrelevant to the filter
    # procedure, but need to be replaces to make signal::filtfilt work
    x_ext[which(is.na(x_ext))] <- 0
    out_pre <- signal::filtfilt(bf, x_ext)
    out <- out_pre[(length(x) + 1):(2 * length(x))]
  } else {
    out <- signal::filter(bf, replace_intna(x))
  }
  as.vector(out)
}

#' Replace internal NAs with interpolated values
#'
#' Internal function for \code{\link{bw_filter}} in \code{\link{spiro_smooth}}
#' @noRd
replace_intna <- function(data, keep_NA_all = FALSE) {
  if (all(is.na(data))) {
    if (keep_NA_all) {
      return(data)
    } else {
      stop("Could not filter data as it only contains NAs.")
    }
  }
  out <- stats::approx(x = seq_along(data), y = data, xout = seq_along(data))
  out$y
}
