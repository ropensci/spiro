#' Return maximum values from cardiopulmonary exercise tests
#'
#' \code{spiro_max()} returns a \code{data.frame} with the maximum gas exchange
#' parameters of an exercise test.
#'
#' Before calculating the maximum values, the raw data is smoothed. Default
#' smoothing method is a 30-second rolling average. See the smooth argument in
#' \code{\link{spiro_smooth}} for more options, such as breath-based averages or
#' digital filtering.
#'
#' Parameters calculated are the maximum oxygen uptake (absolute and relative),
#' carbon dioxide output, minute ventilation, respiratory exchange ratio (RER),
#' and heart rate. The maximum value are defined as the highest single data
#' values after the smoothing. For the maximum RER a different algorithm is
#' used, as the RER during and after rest may exceed the peak value during
#' exercise. Therefore only values during the last ten percent of the exercise
#' time are considered for the RERmax determination. The RERmax calculation
#' works best for data from tests without rest intervals (e.g., ramp tests) and
#' with attached load protocol data.
#'
#' @param data A \code{data.frame} of the class \code{spiro} containing the gas
#'   exchange data. Usually the output of a \code{\link{spiro}} call.
#' @param smooth Parameter giving the filter methods for smoothing the data.
#'   Default is \code{30} for a 30-second moving average. \code{"20b"} will
#'   apply a 20-breath averaging for example. See \code{\link{spiro_smooth}} for
#'   further details and filter methods (e.g. Butterworth filters).
#' @param hr_smooth A logical, whether smoothing should also apply to heart rate
#'   data. Default is `FALSE`, which means that the absolute maximum heart rate
#'   value is taken without smoothing.
#'
#' @return A \code{data.frame} with the maximum parameter values of the data.
#'
#' @examples
#' # Import and process example data sets
#' gxt_data <- spiro(file = spiro_example("zan_gxt"))
#'
#' spiro_max(gxt_data)
#'
#' # Use an averaging over a time interval of 15 seconds
#' spiro_max(gxt_data, smooth = 15)
#'
#' # Use an averaging over an interval of 15 breaths
#' spiro_max(gxt_data, smooth = "15b")
#' @export

spiro_max <- function(data, smooth = 30, hr_smooth = FALSE) {
  # validate hr_smooth input
  if (!is.logical(hr_smooth)) {
    stop("'hr_smooth' must be either TRUE or FALSE")
  }

  # apply smoothing filter
  filt <- spiro_smooth(
    data = data,
    smooth = smooth,
    columns = c("VO2", "VCO2", "VE")
  )

  # calculate maximum values
  maxs <- vapply(filt, max, numeric(1), na.rm = TRUE)
  maxs["VO2_rel"] <- maxs["VO2"] / attr(data, "info")$bodymass

  # Calculating the RERmax is more difficult:
  # Measurements during or directly after rest and cool-down phases may result
  # in RER values higher than the peak during exercise. RERmax is determined as
  # the maximum value of smoothed RER data during the last 10 percent of
  # exercise time excluding rest and cool-down intervals.
  if (nrow(attr(data, "raw")) == nrow(filt)) {
    filt$t <- attr(data, "raw")$time
  } else {
    filt$t <- data$time
  }
  # exclude cool-down or post-exercise rest (only if protocol is available)
  if (any(data$step > 0)) {
    t_end <- max(which(data$step > 0), na.rm = TRUE)
    filt <- filt[filt$t <= t_end, ]
  }
  # only evaluate last 10 percent of exercise time
  t_total <- max(filt$t)
  filt_cut <- filt[filt$t >= 0.9 * t_total, ]
  # remove time filter if no measurements are available during this period
  if (all(is.na(filt_cut$VO2))) filt_cut <- filt
  filt_cut$RER <- filt_cut$VCO2 / filt_cut$VO2
  maxs["RER"] <- max(filt_cut$RER, na.rm = TRUE)

  # check if HR data is available
  if (!all(is.na(data$HR))) {
    if (hr_smooth) {
      # apply smoothing to heart rate data
      hr_sm <- spiro_smooth(
        data = data,
        smooth = smooth,
        columns = "HR"
      )

      # if a breath average is chosen but the raw breath data does not contain
      # HR data this will yield only NAs. In this case the time-based average
      # will be calculated displaying a message.
      if (all(is.na(hr_sm))) {
        hr_sm <- spiro_smooth(
          data = data,
          smooth = attr(filt, "smooth_method")$param,
          columns = "HR"
        )
        message(
          paste0(
            "For heart rate values, time-based smoothing was used instead of",
            " breath-based."
          )
        )
      }
      hr_max <- max(hr_sm, na.rm = TRUE)
    } else {
      hr_max <- max(data$HR, na.rm = TRUE)
    }
    maxs["HR"] <- hr_max
  } else {
    maxs["HR"] <- NA
  }

  out <- as.data.frame(t(maxs))

  # write smoothing method as attribute (useful for reactive environments)
  attr(out, "smooth_method") <- attr(filt, "smooth_method")

  # assign to spiro class (for separate printing methods)
  class(out) <- c("spiro", "data.frame")

  out
}
