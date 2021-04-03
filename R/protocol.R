#' Manually generate a testing protocol for spiroergometry files
#'
#' \code{spiro_protocol_*} manually generates a testing protocol which can be
#' applied to spiroergometry files.
#'
#' This function provides a manual interface for generating testing protocols in
#' exercise science. The shorthands \code{_clt}, \code{_gxt} and
#' \code{_rmp} provide wrappers with pre-defined settings for constant load,
#' graded exercise (i.e. incremental) and ramp tests.
#'
#' To automatically determine the protocol from a data file use
#' \code{guess_protocol} instead.
#'
#'
#' @param step.start A numeric value giving the load of the first (non warm-up)
#'   step.
#' @param step.increment A numeric value, by which load increases with every
#'   new step.
#' @param step.duration An integer giving the duration of each step in seconds.
#' @param rest.duration An integer giving the duration of the rest between the
#'   steps in seconds.
#' @param pre.duration An integer giving the duration of the initial
#'   pre-measurement without load in seconds.
#' @param wu.duration An integer giving the duration of the warm-up step in
#'   seconds.
#' @param wu.load A numeric value giving the load of the warm-up step.
#' @param step.count A numeric value indicating the number of steps. A
#'   non-integer value corresponds to a not finished last step.
#' @param rest.initial A logical, whether there should be a initial rest between
#'   warm-up and first step.
#' @param testtype A character value, usually "ramp", "increment" or
#'   "constant". Per default the testtype is guessed from the test protocol
#'   characteristics.
#' @param ... Passing of the above arguments in the shorthand functions.
#'
#' @return A \code{data.frame} containing the test characteristics, which can be
#'   applied to a given (interpolated) file by \code{apply_protocol}.
#'
#' @export

spiro_protocol <- function(step.start,
                          step.increment,
                          step.duration,
                          rest.duration,
                          pre.duration,
                          wu.duration,
                          wu.load,
                          step.count,
                          rest.initial,
                          testtype = NULL) {

  if (is.null(testtype)) {
    if (step.increment == 0) testtype <- "constant"
    else if (rest.duration == 0 && step.duration <= 90) testtype <- "ramp"
    else testtype <- "increment"
  }

  out <- data.frame(
    step.start,
    step.increment,
    step.duration,
    rest.duration,
    pre.duration,
    wu.duration,
    wu.load,
    step.count,
    rest.initial,
    testtype
  )
  out
}

#' @describeIn spiro_protocol Interface for constant load test protocols. Per
#'   default, six steps of five minutes with 30 seconds rest in between are
#'   applied. Following a one minute pre-measure, the warm-up is of the same
#'   length and half the load of the steps.
#' @export

spiro_protocol_clt <- function(step.start, step.duration = 300, ...) {
  out <- spiro_protocol(
    step.start = step.start,
    step.increment = 0,
    step.duration = step.duration,
    rest.duration = 30,
    pre.duration = 60,
    wu.duration = step.duration,
    wu.load = 0.5 * step.start,
    step.count = 6,
    rest.initial = TRUE,
    testtype = "constant"
  )
  out
}

#' @describeIn spiro_protocol Interface for ramp test protocols. Per
#'   default, 30s-steps without rest in between are applied. Following a
#'   one-minute pre measure , the warm-up has a duration of two minutes and is
#'   directly followed by the first step.
#' @export

spiro_protocol_rmp <- function(step.count,
                              wu.load = 2.8,
                              step.increment = 0.15,
                              step.duration = 30, ...) {
  out <- spiro_protocol(
    step.start = wu.load + step.increment,
    step.increment = step.increment,
    step.duration = step.duration,
    rest.duration = 0,
    pre.duration = 60,
    wu.duration = 120,
    wu.load = wu.load,
    step.count = step.count,
    rest.initial = FALSE,
    testtype = "ramp"
  )
  out
}

#' @describeIn spiro_protocol Interface for graded exercise/incremental step
#'   test protocols. Per default, there is only a one-minute pre-measure with no
#'   warm-up. The default rest between steps is set to 30 seconds.
#' @export

spiro_protocol_gxt <- function(step.count,
                              step.start = 2,
                              step.increment = 0.4,
                              step.duration = 300, ...) {
  out <- spiro_protocol(
    step.start = step.start,
    step.increment = step.increment,
    step.duration = step.duration,
    rest.duration = 30,
    pre.duration = 60,
    wu.duration = 0,
    wu.load = 0,
    step.count = step.count,
    rest.initial = FALSE,
    testtype = "increment"
  )
  out
}
