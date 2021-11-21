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
#' @param data A \code{data.frame} containing the exercise test data.
#' @param weight A numeric value to manually set the participant's body weight.
#'
#' @return A \code{data.frame} containing the data amplified by the additional
#'   variables.
#' @export
add_weight <- function(data, weight = NULL) {
  # find weight in meta data if not specified
  if (is.null(weight)) weight <- attr(data, "info")$weight

  # no weight found
  if (is.na(weight)) {
    stop("No 'weight' specified")
  }

  # calculate data relative to body weight
  data$VO2_rel <- data$VO2 / weight
  data$VCO2_rel <- data$VCO2 / weight

  # calculate running economy if applicable
  # check if protocol was a running exercise
  if (!all(is.null(data$load)) && max(data$load < 30)) {
    data$RE <- (100 / 6) * (data$VO2_rel / data$load)
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
