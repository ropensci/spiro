#' Apply a test protocol to a exercise testing dataset
#'
#' \code{apply_protocol()} adds a predefined test protocol to an existing set of
#' data from an exercise test.
#'
#' @param data A \code{data.frame} containing the exercise testing data
#'   interpolated to seconds.
#' @param protocol A \code{data.frame} containing the test protocol, as created
#'   by \code{\link{set_protocol_manual}} or \code{\link{get_protocol}}.
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
  out <- cbind(add, data[, ! names(data) %in% c("load","incr"), drop = F])
  attr(out,"protocol") <- protocol
  attr(out,"info") <- attr(data,"info")
  attr(out,"testtype") <- attr(protocol,"testtype")
  class(out) <- class(data)
  out
}

#' Guess a test protocol from a corresponding exercise testing dataset
#'
#' \code{get_protocol()} gets the underlying test protocol based on given
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
    load <- data$load[[index]] # extract load for specific index
    vals <- which(data$load != load) # look for different loads
    rest <- vals[vals > index] # filter for later loads
    if (length(rest) != 0) {
      index <- min(vals[vals > index]) # move on to next load change
    } else {
      break() # if no further load change occurs in the data
    }
  }

  # get time and load for every time point when load changes
  changes <- data[values, c("time","load")]

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
    load = changes$load
  )
}

#' Manually setting a testing profile
#'
#' \code{set_protocol_manual()} allows to set any user-defined load profile
#' for an exercise test.
#'
#' @param duration Either a numeric vector containing the duration (in seconds)
#'   load each load step, or a \code{data.frame} containing columns for duration
#'   and load.
#' @param load A numeric vector of the same length as \code{duration} containing
#'   the corresponding load of each step.
#'
#' @examples
#' set_protocol_manual(duration = c(300,120,300,60,300), load = c(3,5,3,6,3))
#'
#' # using a data.frame as input
#' pt_data <- data.frame(
#'   duration = c(180,150,120,90,60,30),
#'   load = c(200,250,300,350,400,450))
#' set_protocol_manual(pt_data)
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

set_protocol_manual.data.frame <- function(duration, load = NULL) {

  # check if data frame has columns names 'duration' and 'load'
  if (any(names(duration) == "duration") && any(names(duration) == "load")) {
    out <- data.frame(
      duration = duration$duration,
      load = duration$load
    )
  # check if data frame has only two colummns
  # first column will be interpreted as duration, second as load
  } else if (ncol(duration) == 2) {
    out <- data.frame(
      duration = duration[1, ],
      load = duration[2, ]
    )
  } else {
    stop("data.frame must contain columns 'duration' and 'load'")
  }
  out
}

#' Extract features from a test protocol
#'
#' \code{protocol_features()} adds characteristic features to the load steps of
#' an exercise testing protocol.
#'
#' @param protocol A \code{data.frame} containing the raw protocol as given by
#'   \code{\link{get_protocol}} or \code{\link{set_protocol_manual}}.

protocol_features <- function(protocol) {
  protocol$type <- NA
  protocol$code <- NA

  if (protocol$load[[1]] == 0) {
    protocol$type[1] <- "pre measures"
    protocol$code[1] <- 0
  }
  d <- diff(protocol$load[protocol$load != 0]) # calculate differences

  if (d[[1]] != d[[2]] && d[[2]] == d[[3]]) { # warm up present
    protocol$type[min(which(protocol$load != 0))]  <- "warm up"
    protocol$code[min(which(protocol$load != 0))]  <- 0.5
  }

  # load steps and rest
  code_i <- 1
  for (i in which(is.na(protocol$type))) {
    if (protocol$load[i] == 0) {
      protocol$type[i] <- "rest"
      protocol$code[i] <- -1
    } else {
      protocol$type[i] <- "load"
      protocol$code[i] <- code_i
      code_i <- code_i + 1 # consecutive numbers for load steps
    }
  }
  # post measures
  if (protocol$load[nrow(protocol)] == 0) {
    protocol$type[nrow(protocol)] <- "post measures"
    protocol$code[nrow(protocol)] <- -2
  }
  protocol
}


#' Guess a exercise test type from a corresponding test protocol
#'
#' \code{get_testtype()} guesses which type of testing protocol a exercise test
#' is.
#'
#' @param protocol A \code{data.frame} containing the test protocol with
#'   features, as given by \code{\link{protocol_features}}.
#'
#' @return A character, either \code{"incremental"}, \code{"ramp"},
#'   \code{"constant"} or \code{"other"}.

get_testtype <- function(protocol) {
  # round load increases to prevent non-exact equality
  d <- round(diff(protocol$load[protocol$type == "load"]),4)
  t <- protocol$duration[protocol$type == "load"]
  if (all(d[-1] == 0)) {
    testtype <- "constant"
  } else if (all(t[-1] < 120)) {
    testtype <- "ramp"
  } else if (all(d[-1] == d[2])) {
    testtype <- "incremental"
  } else {
    testtype <- "other"
  }
  testtype
}

#' Add information to an exercise test protocol
#'
#' \code{process_protocol()} wraps protocol_features() and get_testtype() for
#'
#' @param testtype A character, either \code{"ramp"}, \code{"constant"},
#'   \code{"incremental"} or \code{"other"} for manually setting the test type.
#' @inheritParams protocol_features
#' @export

process_protocol <- function(protocol, testtype = NULL) {
  if (is.null(protocol)) {
    p <- NULL
  } else {
    p <- protocol_features(protocol)
    if (is.null(testtype)) {
      attr(p, "testtype") <- get_testtype(p)
    } else {
      attr(p, "testtype") <- switch(
        ramp = "ramp",
        constant = "constant",
        increment = "increment",
        other = "other",
        stop("testtype needs to be set properly")
      )
    }
  }
  p
}

#' Setting an exercise testing profile
#'
#' \code{set_protocol()} allows to set an load profile for an exercise test
#' based on profile sections.
#'
#' @param ... Functions related to sections of the test profile, such as
#'   \code{pre}, \code{wu}, \code{const} or \code{step}. Sections will be
#'   evaluated in the order they are entered.
#' @param duration A number, giving the duration of the test section or
#'   a single load within the test section (in seconds).
#' @param rest.duration A number, specifying the duration of (each) rest (in
#'   seconds).
#' @param load A number, giving the (initial) load of a section.
#' @param increment A number, giving the difference in load between the current
#'   and the following load step.
#' @param count An integer for the number of load sections.
#' @seealso [set_protocol_manual] for completely manual protocol design.
#'
#' @examples
#' set_protocol(pre(60), wu(300,100), steps(180,150,25,8,30))
#'
#' @export
set_protocol <- function(...) {
  l <- list(...)
  do.call("rbind",l)
}

#' @describeIn set_protocol Add pre-measures to a load protocol
#' @export

pre <- function(duration) {
  data.frame(
    duration = duration,
    load = 0
  )
}

#' @describeIn set_protocol Add a warm up to a load protocol
#' @export

wu <- function(duration, load, rest.duration = 0) {
  if (rest.duration == 0) { # no rest after warm up
    p <- NULL
    l <- NULL
  } else {
    p <- rest.duration
    l <- 0
  }
  data.frame(
    duration = c(duration, p),
    load = c(load, l)
  )
}

#' @describeIn set_protocol Add a stepwise load protocol
#' @export

steps <- function(duration, load, increment, count, rest.duration = 0) {
  rest.load <- 0
  if (rest.duration == 0) {
    rest.load <- NULL
    rest.duration <- NULL
  }
  i <- 1
  l <- load
  ds <- NULL
  ls <- NULL
  # repeatedly bind load (and eventually rest) measures until step count is
  # reached
  while (i <= count) {
    ds <- c(ds, duration, rest.duration)
    ls <- c(ls, l, rest.load)
    l <- l + increment
    i <- i + 1
  }
  d <- data.frame(
    duration = ds,
    load = ls
  )
  if (is.null(rest.load)) d else d[-nrow(d),] # remove last rest interval
}

#' @describeIn set_protocol Add a constant load protocol
#' @export

const <- function(duration, load, count, rest.duration = 0) {
  steps(duration = duration,
       load = load,
       increment = 0,
       count = count,
       rest.duration = rest.duration)
}
