#' Interpolate spiroergometric data
#'
#' \code{spiro_interpolate()} linearly interpolates data from exercise testing
#' to seconds.
#'
#' As spiroergometric data is usually recorded breath-by-breath it comprises
#' different time intervals. For further calculations, a interpolation to every
#' second of the measurement is beneficial.
#'
#' If there a duplicates in the time values (mostly due to rounding or imprecise
#' time measurements) both values will be slightly separated to keep all
#' information for the interpolation.
#'
#' Note that the applied linear interpolation will have a minor effect of
#' smoothing on the data.
#'
#' @param data A \code{data.frame} containing the imported raw data. It is
#'   highly recommended to contain breath-by-breath data to avoid applying
#'   multiple interpolations on the original data.
#'
#' @return A \code{data.frame} with data interpolated to every second.
spiro_interpolate <- function(data) {

  # find and handle duplicates
  # in some rare cases time data may contain duplicates (e.g. when time was
  # rounded).
  data$time <- dupl(data$time)

  # check if data is breath by breath and otherwise display a warning message
  bb <- check_bb(data$time)
  if (!bb) {
    warning(
      paste0("It seems like your data was not recorded breath-by-breath. ",
             "Pre-averaged raw data may result in wrong automated protocol ",
             "guesses and affects the calculation of summary statistics.")
    )
  }

  # interpolate the data
  yout <- vapply(data[-1], spiro_interpolate.internal,
    FUN.VALUE = numeric(round(max(data$time, na.rm = TRUE))),
    x = data$time
  )
  # write time column
  xout <- 1:round(max(data$time, na.rm = TRUE))

  df <- data.frame(time = xout, yout)

  attr(df, "info") <- attr(data, "info") # save meta data as attribute
  class(df) <- c("spiro", "data.frame") # write class
  df
}

#' Row-wise interpolate spiroergometric data
#'
#' Internal function to \code{?link{spiro_interpolate}}
#'
#' @param y,x Numeric vectors giving the data to be interpolate
#' @noRd
spiro_interpolate.internal <- function(y, x) {

  # simple linear interpolation based on time data
  if (all(is.na(y))) {
    dfinter <- rep.int(NA, round(max(x, na.rm = TRUE)))
  } else {
    interpol <- stats::approx(
      y = y,
      x = x,
      xout = 1:round(max(x, na.rm = TRUE))
    )
    dfinter <- interpol$y
  }
  dfinter
}
