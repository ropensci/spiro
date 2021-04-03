spiro_interpolate <- function(data) {
  yout <- sapply(data[-1], spiro_interpolate.internal, x = data$time)
  xout <- 1:round(max(data$time, na.rm = TRUE))
  df <- data.frame(time = xout,yout)
  attr(df, "info") <- attr(data,"info")
  df
}

spiro_interpolate.internal <- function(y, x) {
  interpol <- stats::approx(y = y, x = x, xout = 1:round(max(x, na.rm = TRUE)))
  dfinter <- interpol$y
  dfinter
}

spiro_add <- function(data, weight = NULL) {
  if (is.null(weight)) weight = attr(data, "info")$weight
  data$VO2_rel <- data$VO2 / weight
  data$VCO2_rel <- data$VCO2 / weight
  data$RER <- data$VCO2 / data$VO2
  data$RE <- (100/6) * (data$VO2_rel / data$load)
  for (i in seq_along(data$RE)) {
    if (is.na(data$RE[[i]])) data$RE[[i]] <- NA
    else if (data$RE[[i]] >= 1000) data$RE[[i]] <- NA
  }
  data
}
