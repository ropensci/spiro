#' Manually generate a testing protocol for spiroergometry files
#'
#' \code{spiro_protocol()} generates a testing protocol which can be
#' applied to spiroergometry files.
#'
#' This function provides a manual interface for generating testing protocols in
#' exercise science. The shorthands \code{_clt}, \code{_gxt} and
#' \code{_rmp} provide wrappers with pre-defined settings for constant load,
#' graded exercise (i.e. stepwise incremental) and ramp tests.
#'
#' To automatically determine the protocol from a data file use
#' \code{\link{get_protocol}} instead.
#'
#' To manually supply a protocol with individual load and duration use
#' \code{\link{set_protocol_manual}}
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

apply_protocol <- function(data, protocol) {

  if (is.null(protocol)) { # no protocol given
    add <- data.frame(
      load = rep.int(0, nrow(data)),
      step = rep.int(0, nrow(data))
    )
  } else {
    load <- rep.int(protocol$load, protocol$duration)
    step <- rep.int(protocol$code, protocol$duration)
    add <- data.frame(
      load = load,
      step = step
    )
    if (nrow(data) < nrow(add)) {
      add <- add[1:nrow(data), ]
      rownames(add) <- NULL
    } else if (nrow(data) > nrow(add)) {
      dif <- nrow(data) - nrow(add)
      end <- data.frame(
        load = rep.int(0,dif),
        step = rep.int(-2,dif)
      )
      add <- rbind(add, end)
    }
  }
  out <- cbind(add, data[, ! names(data) %in% c("velocity","incr"), drop = F])
  attr(out,"protocol") <- protocol
  attr(out,"info") <- attr(data,"info")
  out
}

#' Guess a test protocol from a corresponding exercise testing dataset
#'
#' \code{guess_protocol()} gets the underlying test protocol based on given
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
#' get_protocol(raw_data)
#' @export

get_protocol <- function(data) {

  # get indices for changes in load
  index <- 1
  values <- NULL
  while (index <= nrow(data)) {
    values <- c(values, index)
    load <- data$velocity[[index]] # extract load for specific index
    vals <- which(data$velocity != load) # look for different loads
    rest <- vals[vals > index] # filter for later loads
    if (length(rest) != 0) {
      index <- min(vals[vals > index]) # move on to next load change
    } else {
      break() # if no further load change occurs in the data
    }
  }

  changes <- data[values, c("time","velocity")]

  # calculate duration of each load step
  duration <- rep.int(NA,length(values))
  for (i in seq_along(values)) {
    if (i < length(values)) {
      duration[i] <- changes$time[[i+1]] - changes$time[[i]]
    } else { # for last load
      duration[i] <- max(data$time, na.rm = TRUE) - changes$time[[i]]
    }
  }

  data.frame(
    duration = round(duration,-1), # round to full 10 seconds
    load = changes$velocity
  )
}

#' Manually setting a testing profile
#'
#' \code{set_protocol_manually()} allows to set any user-defined load profile
#' for an exercise test.
#'
#' @export

set_protocol_manual <- function(duration, load = NULL) {
  UseMethod("set_protocol_manual")
}

#' @describeIn set_protocol_manual Default method when duration and load are
#'   given separately
#' @export

set_protocol_manual.default <- function(duration, load) {
  if (length(duration) != length(load)) {
    stop("duration and load must be vectors of the same length")
  }
  data.frame(
    duration = duration,
    load = load
  )
}

#' @describeIn set_protocol_manual Method for data.frames with duration and load
#'   column
#' @export

set_protocol_manual.data.frame <- function(data) {

  if (any(names(dd) == "duration") && any(names(dd) == "load")) {
    out <- data.frame(
      duration = data$duration,
      load = data$load
    )
  } else if (ncol(data) == 2) {
    out <- data.frame(
      duration = data[1, ],
      load = data[2, ]
    )
  } else {
    stop("data.frame must contain columns duration and load")
  }
  out
}

protocol_features <- function(data) {
  data$type <- NA
  data$code <- NA

  if (data$load[[1]] == 0) {
    data$type[1] <- "pre measures"
    data$code[1] <- 0
  }
  d <- diff(data$load[data$load != 0]) # calculate differences

  if (d[[1]] != d[[2]] && d[[2]] == d[[3]]) { # warm up present
    data$type[min(which(data$load != 0))]  <- "warm up"
    data$code[min(which(data$load != 0))]  <- 0.5
  }

  # load steps and rest
  code_i <- 1
  for (i in which(is.na(data$type))) {
    if (data$load[i] == 0) {
      data$type[i] <- "rest"
      data$code[i] <- -1
    } else {
      data$type[i] <- "load"
      data$code[i] <- code_i
      code_i <- code_i + 1 # consecutive numbers for load steps
    }
  }
  # post measures
  if (data$load[nrow(data)] == 0) {
    data$type[nrow(data)] <- "post measures"
    data$code[nrow(data)] <- -2
  }

  data
}
