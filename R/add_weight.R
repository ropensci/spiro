#' Calculate additional variables related to body weight for cardiopulmonary
#' exercise testing data
#'
#' \code{add_weight()} amplifies existing spiroergometric data by the
#' calculation of body weight-related variables.
#'
#' Based on the participant's body weight relative oxygen uptake (VO2_rel) and
#' carbon dioxide (VCO2_rel) output are calculated. \code{weight} will be
#' received from the original file's meta data, but can be manually overridden,
#' e.g. if the value in the meta data is incorrect or rounded.
#'
#' For running protocols, running economy (RE) is calculated.
#'
#' @param weight A numeric value to manually set the participant's body weight.
#'
#' @inheritParams spiro_max
#'
#' @return A \code{data.frame} of the class \code{spiro} containing the
#'   cardiopulmonary exercise testing data including variables relative to body
#'   weight.
#' @examples
#' # get example file
#' file <- spiro_example("zan_gxt")
#'
#' s <- spiro(file)
#' add_weight(s, weight = 65.3)
#' @export
add_weight <- function(data, weight = NULL) {

  if (!is.null(weight)) {
    if (!is.numeric(weight)) {
      stop("'weight' must be a numeric value")
    } else if (weight <= 0) {
      stop("'weight' must be greater than 0")
    }
  } else {
    weight <- attr(data, "info")$weight
  }

  # no weight found
  if (is.na(weight)) {
    stop("No 'weight' specified")
  }

  # calculate data relative to body weight
  data$VO2_rel <- data$VO2 / weight
  data$VCO2_rel <- data$VCO2 / weight

  # calculate running economy if applicable
  # check if protocol was a running exercise
  if (!all(is.null(data$load)) && (max(data$load) < 30)) {
    data$RE <- (100 / 6) * (data$VO2_rel / data$load)
    for (i in seq_along(data$RE)) { # result NAs for rest sections
      if (is.na(data$RE[[i]])) {
        data$RE[[i]] <- NA
      } else if (data$RE[[i]] >= 1000) {
        data$RE[[i]] <- NA
      }
    }
  }

  # save possible new weight data in the meta attribute
  attr(data, "info")$weight <- weight

  data
}
