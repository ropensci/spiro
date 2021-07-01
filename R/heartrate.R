#' Import and apply heart rate data to a spiroergometric data file
#'
#' \code{hr_add()} imports an additional file containing heart rate data and
#' adds it to an existing spiroergometric data file.
#'
#' Heart rate data will be imported from a \code{.tcx} file with
#' \code{hr_import()}. After interpolating the data to full seconds, it is then
#' applied to a given data set by \code{hr_apply()}.
#'
#' @param data A \code{data.frame} containing spiroergometric data interpolated
#'   to every second.
#' @param hr_data A data.frame containing the imported heart rate data.
#' @inheritParams spiro
#'
#' @return A \code{data.frame} containing the spiroergometric and heart rate
#'   data.
#'
#' @export

hr_add <- function(data, hr_file, hr_offset = 0) {
  hr_data <- hr_import(hr_file)
  data <- hr_apply(data, hr_data = hr_data, hr_offset = hr_offset)
  data
}

#' @rdname hr_add
hr_import <- function(hr_file) {
  filepath <- get_path(hr_file)
  tcx_data <- XML::xmlParse(filepath)
  tcx_raw <- XML::xmlToDataFrame(
    nodes = XML::getNodeSet(tcx_data, "//ns:Trackpoint", "ns"))
  hr <- hr_interpolate(tcx_raw)
  hr
}

#' @rdname hr_add
hr_apply <- function(data, hr_data, hr_offset = 0) {
  if (hr_offset < 0) {
    hr_prewhile <- hr_data[-1:pre_time]
  } else {
    hr_prewhile <- c(rep(NA, hr_offset),hr_data)
  }

  if (length(hr_prewhile) >= nrow(data)) {
    data$HR <- as.numeric(hr_prewhile[1:nrow(data)])
  } else {
    mis <- nrow(data) - length(hr_prewhile)
    data$HR <- as.numeric(c(hr_prewhile, rep(NA, mis)))
  }
  data
}

hr_interpolate <- function(data) {
  # get time data from tcx
  dt <- sapply(data$Time, gettime, USE.NAMES = FALSE)
  # convert to seconds
  ds <- to_seconds(dt)
  # handle duplicated values
  time <- dupl(ds - (ds[[1]]-1))
  # perform linear interpolation
  hr <- stats::approx(x = time,
                      y = data$HeartRateBpm,
                      xout = seq.int(1,max(time),1))$y
  hr
}

gettime <- function(text) {
  regmatches(text,regexpr("\\d\\d\\:\\d\\d\\:\\d\\d",
                          text))
}
