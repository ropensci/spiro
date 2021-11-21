#' Import and apply heart rate data to cardiopulmonary exercise testing data
#'
#' \code{add_hr()} imports an additional file containing heart rate data and
#' adds it to an existing spiroergometric data file.
#'
#' Heart rate data will be imported from a \code{.tcx} file. After interpolating
#' the data to full seconds, it is then matched to the imported data.
#'
#' @param data A \code{data.frame} containing spiroergometric data interpolated
#'   to every second.
#' @inheritParams spiro
#'
#' @return A \code{data.frame} containing the spiroergometric and heart rate
#'   data.
#'
#' @examples
#' # Get example data
#' oxy_file <- spiro_example("zan_ramp")
#' hr_file <- spiro_example("hr_ramp.tcx")
#'
#' # Import and process spiro data
#' oxy_data <- spiro(oxy_file)
#'
#' # Add heart rate data
#' add_hr(oxy_data, hr_file)
#' @export

add_hr <- function(data, hr_file, hr_offset = 0) {
  # import heart rate data
  hr_data <- hr_import(hr_file)

  # handle beginning of data
  if (hr_offset < 0) {
    # if heart rate measures started before gas exchange measures:
    # cut first part of heart rate data
    hr_prewhile <- hr_data[-1:hr_offset]
  } else {
    # if heart rate measures started after gas exchange measures:
    # write NAs for the first heart rate data points
    hr_prewhile <- c(rep(NA, hr_offset), hr_data)
  }

  # handle end of data
  if (length(hr_prewhile) >= nrow(data)) {
    # if heart rate measures ended after gas exchange measures:
    # cut end of heart rate data
    data$HR <- as.numeric(hr_prewhile[seq_len(nrow(data))])
  } else {
    # if heart rate measures ended before gas exchange measures:
    # write NAs for the last heart rate data points
    mis <- nrow(data) - length(hr_prewhile)
    data$HR <- as.numeric(c(hr_prewhile, rep(NA, mis)))
  }
  data
}

hr_import <- function(hr_file) {

  # currently working for Garmin .tcx files

  # -- TO DO --
  # check if it works for other types of heart rate data files and rewrite
  # accrodingly

  tcx_data <- XML::xmlParse(hr_file)
  tcx_raw <- XML::xmlToDataFrame(
    nodes = XML::getNodeSet(tcx_data, "//ns:Trackpoint", "ns")
  )
  hr <- hr_interpolate(tcx_raw)
  hr
}

hr_interpolate <- function(data) {
  # get time data from tcx
  dt <- vapply(data$Time, gettime, FUN.VALUE = character(1), USE.NAMES = FALSE)
  # convert to seconds
  ds <- to_seconds(dt)
  # handle duplicated values
  time <- dupl(ds - (ds[[1]] - 1))
  # perform linear interpolation
  hr <- stats::approx(
    x = time,
    y = data$HeartRateBpm,
    xout = seq.int(1, max(time), 1)
  )$y
  hr
}

gettime <- function(text) {
  regmatches(text, regexpr(
    "\\d\\d\\:\\d\\d\\:\\d\\d",
    text
  ))
}
