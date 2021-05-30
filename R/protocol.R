#' Manually generate a testing protocol for spiroergometry files
#'
#' \code{spiro_protocol()} manually generates a testing protocol which can be
#' applied to spiroergometry files.
#'
#' This function provides a manual interface for generating testing protocols in
#' exercise science. The shorthands \code{_clt}, \code{_gxt} and
#' \code{_rmp} provide wrappers with pre-defined settings for constant load,
#' graded exercise (i.e. stepwise incremental) and ramp tests.
#'
#' To automatically determine the protocol from a data file use
#' \code{\link{guess_protocol}} instead.
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
#'   applied to a given (interpolated) file by \code{\link{apply_protocol}}.
#'
#' @examples
#' spiro_protocol(step.start = 150, step.increment = 30, step.duration = 180,
#'                rest.duration = 0, pre.duration = 60, wu.duration = 0,
#'                wu.load = 0, step.count = 7, rest.initial = FALSE)
#'
#' # which can be simplified:
#' spiro_protocol_gxt(step.count = 7, step.start = 150, step.increment = 30,
#'                    step.duration = 180, rest.duration = 30)
#'
#' spiro_protocol_rmp(step.count = 19, wu.load = 3.0, step.increment = 0.2)
#'
#' spiro_protocol_clt(step.start = 4.5)
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
    if (step.increment == 0) {
      testtype <- "constant"
    } else if (rest.duration == 0 && step.duration <= 90) {
      testtype <- "ramp"
    } else {
      testtype <- "increment"
    }
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

#' Apply a test protocol to a exercise testing dataset
#'
#' \code{apply_protocol()} adds a predifined test protocol to an existing set of
#' data from an exercise test.
#'
#' @param data A \code{data.frame} containing the exercise testing data
#'   interpolated to seconds.
#' @param protocol A \code{data.frame} containing the test protocol, as created
#'   by \code{\link{spiro_protocol}} or \code{\link{guess_protocol}}
#'
#' @examples
#' # Import and Interpolate example data
#' raw_data <- spiro_import(file = spiro_example("zan_gxt"))
#' data <- spiro_interpolate(raw_data)
#'
#' apply_protocol(data, protocol = spiro_protocol_clt(4.5, step.count = 7))
#' @export

apply_protocol <- function(data,protocol) {

  if (length(protocol) != 10) {
    velocity <- NULL # define variables to omit missing global binding error
    incr <- NULL
    out <- data.frame(
      load = rep.int(0, length(data$time)),
      step = rep.int(0, length(data$time)),
      data[, ! names(data) %in% c("velocity","incr"), drop = F])
    attr(out,"protocol") <- NA
    attr(out,"info") <- attr(data,"info")
    return(out)
  }

  pre <- rep.int(0, protocol$pre.duration)
  wu <- rep.int(protocol$wu.load, protocol$wu.duration)
  wuN <- rep.int(0.5, protocol$wu.duration)
  rest <- rep.int(0, protocol$rest.duration)
  restN <- rep.int(-1, protocol$rest.duration)
  if (protocol$rest.initial) {
    rest.initial <- rest
    rest.initialN <- restN
  } else {
    rest.initial <- NULL
    rest.initialN <- NULL
  }
  i <- 1
  z <- protocol$step.start
  out <- NULL
  outN <- NULL

  while (i <= trunc(protocol$step.count)) {
    run <- rep.int(z,  protocol$step.duration)
    runN <- rep.int(i, protocol$step.duration)
    if (i == 1) {
      step <- c(rest.initial, run)
      stepN <- c(rest.initialN, runN)
    } else {
      step <- c(rest, run)
      stepN <- c(restN, runN)
    }
    out <- c(out, step)
    outN <- c(outN, stepN)
    z <- z + protocol$step.increment
    i <- i + 1
  }

  # if last step was not finished
  if ((protocol$step.count - trunc(protocol$step.count)) != 0) {
    lastduration <- round(
      protocol$step.duration * (protocol$step.count -
                                trunc(protocol$step.count)),-1)
    last <- rep.int(z, lastduration)
    lastN <- rep.int(i,lastduration)
    out <- c(out, rest, last)
    outN <- c(outN, restN, lastN)
  }
  outtest <- c(pre,wu,out)
  posttime <- length(data$time) - length(outtest)
  if (posttime <= 0) {
    out <- data.frame(
      load = outtest[1:length(data$time)],
      step = c(pre, wuN, outN)[1:length(data$time)],
      data)
  } else {
    post <- rep.int(0, posttime)
    postN <- rep.int(-2, posttime)
    out <- data.frame(
      load = c(outtest, post),
      step = c(pre, wuN, outN, postN),
      data[, ! names(data) %in% c("velocity","incr"), drop = F])
  }
  attr(out, "info") <- attr(data,"info")
  attr(out, "protocol") <- protocol
  class(out) <- c("spiro","data.frame")
  out
}

#' Guess a test protocol from a corresponding exercise testing dataset
#'
#' \code{guess_protocol()} guesses the underlying test protocol based on given
#' load data.
#'
#' @param data A \code{data.frame} containing the exercise testing data. It is
#'   highly recommend to parse non-interpolated breath-by-breath data.
#'
#' @return A \code{data.frame} containing the characteristics of the test
#'   protocol.
#' @examples
#' # Import example data
#' raw_data <- spiro_import(file = spiro_example("zan_gxt"))
#'
#' guess_protocol(raw_data)
#' @export

guess_protocol <- function(data) {

  rest.initial <- NULL

  nonnulls <- which(data$velocity != 0)
  firstload <- min(nonnulls)
  if (firstload != 1) {
    pre.duration <- round(
      (data$time[[firstload]] + data$time[[firstload-1]])/2,-1)
  } else {
    pre.duration <- 0
  }
  load1 <- round(data$velocity[[firstload]],2)

  nulls <- which(data$velocity == 0)
  nonloads1 <- which(data$velocity != load1)
  nextload <- min(nonloads1[nonloads1 > firstload])

  if (data$velocity[[nextload]] != 0){ #no rest
    rest.duration <- 0
    nextload_timepoint <- round(
      (data$time[[nextload]] + data$time[[nextload-1]])/2,-1)
    load1_time <-  nextload_timepoint - pre.duration
    load2 <- round(data$velocity[[nextload]],2)
    nonloads2 <- which(data$velocity != load2)
    nextload2 <- min(nonloads2[nonloads2 > nextload])
    nextload2_timepoint <- round(
      (data$time[[nextload2]] + data$time[[nextload2]])/2,-1)
    load2_time <- nextload2_timepoint - nextload_timepoint
    load3 <- round(data$velocity[[nextload2]],2)
    rest.initial <- FALSE
  } else {
    firstrest <- min(nulls[nulls > firstload])
    firstrest_point <- round(
      (data$time[[firstrest]] + data$time[[firstrest-1]])/2,-1)
    load1_time <- firstrest_point - pre.duration

    secondload <- min(nonnulls[nonnulls > firstrest])
    secondload_timepoint <- round(
      (data$time[[secondload]] + data$time[[secondload-1]])/2,-1)
    rest.duration <- secondload_timepoint - firstrest_point
    load2 <- round(data$velocity[[secondload]],2)

    secondrest <- min(nulls[nulls > secondload])
    secondrest_point <- round(
      (data$time[[secondrest]] + data$time[[secondrest-1]])/2,-1)
    load2_time <- secondrest_point - secondload_timepoint

    thirdload <- min(nonnulls[nonnulls > secondrest])
    load3 <- round(data$velocity[[thirdload]],2)
  }

  delta_load12 <- load2 - load1
  delta_load23 <- load3 - load2

  if (delta_load12 == delta_load23 && load1_time == load2_time) { # no warm up
    step.increment <- delta_load12
    step.start <- load1
    step.duration <- load1_time
    rest.duration <- rest.duration
    wu.duration <- 0
    wu.load <- 0
    rest.initial <- FALSE
  } else {
    step.increment <- delta_load23
    wu.duration <- load1_time
    wu.load <- load1
    step.start <- load2
    step.duration <- load2_time
    rest.duration <- rest.duration
    if (is.null(rest.initial)) rest.initial <- TRUE
  }

  end.timepoint <- round(data$time[[max(nonnulls)]],-1)
  testtime <- end.timepoint - pre.duration - wu.duration
  step.count <- trunc(testtime / (step.duration + rest.duration),0)
  step_modulo <- (testtime %% (step.duration + rest.duration) / step.duration)
  step.count <- round(step.count + step_modulo,2)

  if (step.increment == 0) {
    testtype <- "constant"
  } else if (rest.duration == 0 && step.duration <= 90) {
    testtype <- "ramp"
  } else {
    testtype <- "increment"
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
