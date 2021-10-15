#' Interpolate spiroergometric data
#'
#' \code{spiro_interpolate()} linearly interpolates data from exercise testing
#' to seconds.
#'
#' As spiroergometric data is usually recorded breath-by-breath it spans erratic
#' time intervals. For further calculations, a interpolation to every second of
#' the measurement is beneficial.
#'
#' If there a duplicates in the data's time values (mostly due to rounding or
#' imprecise time measurements) both values will be slightly separated to keep
#' all information for the interpolation.
#'
#' Note that the applied linear interpolation will have a minor smoothing
#' effect on the data.
#'
#' @param data A \code{data.frame} containing the imported raw data. It is
#'   highly recommended to contain breath-by-breath data to avoid applying
#'   multiple interpolations on the original data.
#'
#' @return A \code{data.frame} with data interpolated to every second.
#'
#' @examples
#' # Import example data
#' raw_data <- spiro_import(file = spiro_example("zan_gxt"))
#'
#' spiro_interpolate(raw_data)
#' @export
spiro_interpolate <- function(data) {

  # find and handle duplicates
  # in some rare cases time data may contain duplicates (e.g. when time was
  # rounded).
  data$time <- dupl(data$time)

  # interpolate the data
  yout <- sapply(data[-1], spiro_interpolate.internal, x = data$time)
  # write time column
  xout <- 1:round(max(data$time, na.rm = TRUE))

  df <- data.frame(time = xout,yout)

  attr(df, "info") <- attr(data,"info") # save meta data as attribute
  class(df) <- c("spiro","data.frame") # write class
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
  interpol <- stats::approx(y = y, x = x, xout = 1:round(max(x, na.rm = TRUE)))
  dfinter <- interpol$y
  dfinter
}


#' Calculated additional variables related to body weight to a spiroergometric
#' data set
#'
#' \code{add_weight()} amplifies existing spiroergometric data by calculation of
#' additional variables.
#'
#' Based on the participant's body weight relative oxygen uptake (VO2_rel) and
#' carbon dioxide (VCO2_rel) output are calculated. \code{weight} can be
#' manually overridden, e.g. if the meta-data's value is incorrect or rounded.
#'
#' For running protocols, running economy (RE) is calculated.
#'
#' @param data A \code{data.frame} containing the exercise test data.
#' @param weight A numeric value to manually set the participant's body weight.
#'
#' @return A \code{data.frame} containing the data amplified by the additional
#'   variables.
#' @export

add_weight <- function(data, weight = NULL) {
  # find weight in meta data if not specified
  if (is.null(weight)) weight = attr(data, "info")$weight

  # no weight found
  if (is.na(weight))
    stop("No 'weight' specified")

  # calculate data relative to body weight
  data$VO2_rel <- data$VO2 / weight
  data$VCO2_rel <- data$VCO2 / weight

  # calculate running economy if applicable
  # check if protocol was a running exercise
  if (!all(is.null(data$load)) && max(data$load < 30)) {
    data$RE <- (100/6) * (data$VO2_rel / data$load)
    for (i in seq_along(data$RE)) { # result NAs for rest sections
      if (is.na(data$RE[[i]])) {
        data$RE[[i]] <- NA
      } else if (data$RE[[i]] >= 1000) {
        data$RE[[i]] <- NA
      }
    }
  }

  data
}

#' Handle duplicates for spiroergometric data interpolation
#'
#' Internal function to \code{?link{spiro_interpolate}}
#'
#' To ensure that any time duplicates in a dataset will still be correctly
#' interpolated, the existing duplicates are slightly separated.
#'
#' @param values Numeric vector to check for duplicates
#' @noRd
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

#' Calculate calometric values from gas exchange data
#'
#' Internal function to \code{?link{spiro_add}}
#'
#' Calculates the rates of carbohydrate and fat oxidation (in grams per minute)
#' from oxygen uptake and carbon-dioxide output data using the formula from
#' Peronnet (1991).
#'
#' @param df data.frame with data from cardiopulmonary exercise testing
#' @noRd

calo <- function(data) {
  m <- mapply(FUN = calo.internal, vo2abs = data$VO2, vco2abs = data$VCO2)
  out <- cbind(data,round(apply(t(m),2,unlist),2))

  # preserve class and attributes
  class(out) <- class(data)
  attr(out,"info") <- attr(data,"info")
  attr(out,"protocol") <- attr(data,"protocol")
  attr(out,"raw") <- attr(data,"raw")
  attr(out,"testtype") <- attr(data,"testtype")
  out
}

calo.internal <- function(vo2abs,vco2abs) {
  if (is.na(vo2abs) | is.na(vco2abs)){
    fo <- NA
    cho <- NA
  } else {
    cho <- (vco2abs/1000) * 4.585 - ((vo2abs/1000) * 3.226)
    fo <- ((vo2abs/1000) * 1.695) - ((vco2abs/1000) * 1.701)
    if (fo < 0) fo <- 0
  }
  list(CHO = cho,FO = fo)
}
