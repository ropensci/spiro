spiro_interpolate <- function(data) {
  data$time <- dupl(data$time)
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
  if (is.na(weight))
    stop("No 'weight' specified")
  data$VO2_rel <- data$VO2 / weight
  data$VCO2_rel <- data$VCO2 / weight
  data$RER <- data$VCO2 / data$VO2
  if (!all(is.null(data$load)) && max(data$load < 30)) {
    data$RE <- (100/6) * (data$VO2_rel / data$load)
    for (i in seq_along(data$RE)) {
      if (is.na(data$RE[[i]])) {
        data$RE[[i]] <- NA
      } else if (data$RE[[i]] >= 1000) {
        data$RE[[i]] <- NA
      }
    }
  }
  data
}

dupl <- function(values) {
  d <- anyDuplicated(values)
  if (d != 0) {
    ds <- which(duplicated(values))
    for (di in ds) {
      values[[di-1]] <- (values[[di-1]]-0.1)
      values[[di]] <- (values[[di]]+0.1)
    }
  }
  values
}
