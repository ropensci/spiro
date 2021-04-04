hr_add <- function(data, hr_file, offset = 0) {
  hr_data <- hr_import(hr_file)
  data <- apply_hr(data, hr_data = hr_data, offset = offset)
  data
}


hr_import <- function(file) {
  filepath <- get_path(file)
  tcx_data <- XML::xmlParse(filepath)
  tcx_raw <- XML::xmlToDataFrame(nodes = XML::getNodeSet(tcx_data, "//ns:Trackpoint", "ns"))
  hr <- tcx_raw$HeartRateBpm
  hr
}

apply_hr <- function(data, hr_data, offset = 0) {
  pre_time <- attr(data, "protocol")$pre.duration + offset
  if (pre_time < 0) {
    hr_prewhile <- hr_data[-1:pre_time]
  } else {
    hr_prewhile <- c(rep(NA, pre_time),hr_data)
  }

  if (length(hr_prewhile) >= nrow(data)) {
    data$HR <- as.numeric(hr_prewhile[1:nrow(data)])
  } else {
    mis <- nrow(data) - length(hr_prewhile)
    data$HR <- as.numeric(c(hr_prewhile, rep(NA, mis)))
  }
  data
}
