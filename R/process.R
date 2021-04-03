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
