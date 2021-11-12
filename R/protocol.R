#' Add a test protocol to an exercise testing dataset
#'
#' \code{add_protocol()} adds a predefined test protocol to an existing set of
#' data from an exercise test.
#'
#' @param data A spiro \code{data.frame} containing the exercise testing data.
#' @param protocol A \code{data.frame} containing the test protocol, as created
#'   by \code{\link{set_protocol}}, \code{\link{set_protocol_manual}} or
#'   \code{\link{get_protocol}}.
#'
#'
#' @examples
#' # Get example data
#' file <- spiro_example("zan_gxt")
#'
#' spiro(file) |>
#'   add_protocol(set_protocol(pre(60), steps(300, 50, 50, 7, 30)))
#' @seealso [set_protocol] for protocol setting with helper functions.
#' @seealso [set_protocol_manual] for manual protocol design.
#' @seealso [get_protocol] For automated extracting of protocols from raw data.
#' @export

add_protocol <- function(data, protocol) {


  # attach the protocol to the data frame
  if (is.null(protocol)) { # no protocol given
    add <- data.frame(
      load = rep.int(0, nrow(data)),
      step = rep.int(0, nrow(data))
    )
    ptcl <- NULL
  } else {
    # preprocess the protocol
    ptcl <- get_features(protocol)

    # write load and step code vectors
    add <- data.frame(
      load = rep.int(ptcl$load, ptcl$duration),
      step = rep.int(ptcl$code, ptcl$duration)
    )
    if (nrow(data) < nrow(add)) { # protocol longer than data
      add <- add[1:nrow(data), ] # remove last protocol values
      rownames(add) <- NULL
    } else if (nrow(data) > nrow(add)) { # protocol shorter than data
      dif <- nrow(data) - nrow(add)
      end <- data.frame( # code last seconds as post measures
        load = rep.int(0, dif),
        step = rep.int(-2, dif)
      )
      add <- rbind(add, end)
    }
  }

  # add protocol variables to the existing data
  out <- cbind(add, data[, !names(data) %in% c("load", "step"), drop = FALSE])

  # preserve and create attributes
  attr(out, "info") <- attr(data, "info")
  attr(out, "protocol") <- ptcl
  attr(out, "raw") <- attr(data, "raw")
  attr(out, "testtype") <- get_testtype(ptcl)
  testtype_class <- switch(attr(out, "testtype"),
    "constant" = "spiro_clt",
    "ramp" = "spiro_rmp",
    "incremental" = "spiro_gxt",
    NULL
  )
  class(out) <- c(testtype_class, class(data))

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
#'
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
  changes <- data[values, c("time", "load")]

  # calculate duration of each load step
  duration <- rep.int(NA, length(values))
  for (i in seq_along(values)) {
    if (i < length(values)) {
      duration[i] <- changes$time[[i + 1]] - changes$time[[i]]
    } else { # for last load
      duration[i] <- max(data$time, na.rm = TRUE) - changes$time[[i]]
    }
  }

  data.frame(
    duration = round(duration, -1), # round to full 10 seconds
    load = changes$load
  )
}

#' Extract features from an exercise test protocol
#'
#' \code{get_features()} adds characteristic features to the load steps of
#' an exercise testing protocol.
#'
#' @param protocol A \code{data.frame} containing the raw protocol as given by
#'   \code{\link{get_protocol}}, \code{\link{set_protocol}} or
#'   \code{\link{set_protocol_manual}}.

get_features <- function(protocol) {

  # -- TO DO --
  # rewrite so that it does also work for special protocols (e.g. only a few
  # protocol steps)

  # create empty columns
  protocol$type <- NA
  protocol$code <- NA

  if (protocol$load[[1]] == 0) {
    protocol$type[1] <- "pre measures"
    protocol$code[1] <- 0
  }
  d <- diff(protocol$load[protocol$load != 0]) # calculate differences

  # check if differences between steps are all equal
  # if first difference is unusual, this suggests that a warm-up is present
  if (d[[1]] != d[[2]] && d[[2]] == d[[3]]) {
    protocol$type[min(which(protocol$load != 0))] <- "warm up"
    protocol$code[min(which(protocol$load != 0))] <- 0.5
  }

  # write load steps and rest
  code_i <- 1
  for (i in which(is.na(protocol$type))) {
    if (protocol$load[i] == 0) { # no load means rest
      protocol$type[i] <- "rest"
      protocol$code[i] <- -1
    } else {
      protocol$type[i] <- "load"
      protocol$code[i] <- code_i
      code_i <- code_i + 1 # consecutive numbers for load steps
    }
  }
  # post measures
  if (protocol$load[nrow(protocol)] == 0) { # last load
    protocol$type[nrow(protocol)] <- "post measures"
    protocol$code[nrow(protocol)] <- -2
  }
  protocol
}


#' Guess the type of exercise test protocol
#'
#' \code{get_testtype()} guesses which type of testing protocol a exercise test
#' used.
#'
#' @param protocol A \code{data.frame} containing the test protocol with
#'   features, as given by \code{\link{get_features}}.
#'
#' @return A character, either \code{"incremental"}, \code{"ramp"},
#'   \code{"constant"} or \code{"other"}.

get_testtype <- function(protocol) {
  if (is.null(protocol)) {
    testtype <- "unknown"
  } else {
    # round load increases to prevent non-exact equality
    d <- round(diff(protocol$load[protocol$type == "load"]), 4)
    t <- protocol$duration[protocol$type == "load"]
    if (all(d[-1] == 0)) { # no load changes
      testtype <- "constant"
    } else if (all(t[-1] < 120)) { # load steps shorter than 120 seconds
      testtype <- "ramp"
    } else if (all(d[-1] == d[2])) { # same increment for all steps
      testtype <- "incremental"
    } else {
      testtype <- "other"
    }
  }
  testtype
}


#' Setting an exercise testing profile
#'
#' \code{set_protocol()} allows to set an load profile for an exercise test
#' based on profile sections.
#'
#' @param ... Functions related to sections of the load profile, such as
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
#' @seealso [set_protocol_manual] for manual protocol design.
#' @seealso [get_protocol] For automated extracting of protocols from raw data.
#'
#' @examples
#' set_protocol(pre(60), wu(300, 100), steps(180, 150, 25, 8, 30))
#' @export
set_protocol <- function(...) {
  l <- list(...)
  do.call("rbind", l)
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
  # repeatedly binds load (and eventually rest) measures until step count is
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
  if (is.null(rest.load)) d else d[-nrow(d), ] # remove last rest interval
}

#' @describeIn set_protocol Add a constant load protocol
#' @export

const <- function(duration, load, count, rest.duration = 0) {
  steps(
    duration = duration,
    load = load,
    increment = 0,
    count = count,
    rest.duration = rest.duration
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
#' set_protocol_manual(duration = c(300, 120, 300, 60, 300), load = c(3, 5, 3, 6, 3))
#'
#' # using a data.frame as input
#' pt_data <- data.frame(
#'   duration = c(180, 150, 120, 90, 60, 30),
#'   load = c(200, 250, 300, 350, 400, 450)
#' )
#'
#' set_protocol_manual(pt_data)
#' @seealso [set_protocol] for protocol setting with helper functions.
#' @seealso [get_protocol] For automated extracting of protocols from raw data.
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

#' @describeIn set_protocol_manual Method for data frames with a duration and a
#'   load column
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
