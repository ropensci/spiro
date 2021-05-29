#' Process raw data from metabolic carts/spiroergometric measures
#'
#' \code{spiro()} wraps multiple functions to import, process and amplify raw data
#' from metabolic carts into a \code{data.frame}.
#'
#' This function performs multiple operations on raw data from metabolic carts.
#' It imports the raw data from a file, which might be complemented by an
#' additional \code{.tcx} file with heart rate data. For details of data import
#' and supported file types visit \code{\link{spiro_import}}.
#'
#' Breath-by-breath data is linearly interpolated to get data points for every
#' second. Per default, based on the given velocity data, the underlying testing
#' protocol is guessed and applied to the data. If no load data is available or
#' the protocol guess turns wrong, there is an option to manually specify the
#' test \code{protocol} by setting one of the \code{\link{spiro_protocol}}
#' functions as an argument.
#'
#' Additional variables of gas exchange are calculated for further analysis. Per
#' default the body weight saved in the file's metadata is used for calculating
#' relative measures. It is possible to supply \code{weight} manually to the
#' function, overriding that value.
#'
#' @param file The name of the file which contains the spiroergometry data. The
#'   file  will be found by partial matching of the regular expression within
#'   the working directory (and one level above). Alternatively, \code{file} can
#'   be an absolute or relative path.
#' @param device A character string, specifying the device for measurement. By
#'   default the device type is guessed by the characteristics of \code{file}.
#'   This can be overridden by setting the argument to \code{"zan"} or
#'   \code{"cosmed"}.
#' @param weight Numeric value for participant's body weight, if the default
#'   value saved in \code{file} should be overridden.
#' @param hr_file The name of a \code{*tcx} file which contains additional heart
#'   rate data. The file  will be found by partial matching of the regular
#'   expression within the working directory (and one level above).
#'   Alternatively, \code{hr_file} can be an absolute or relative path.
#' @param hr_offset An integer, corresponding to the temporal offset of the
#'   heart-rate file. By default the start of the heart rate measurement is
#'   linked to the first load in the test protocol (i.e. the end of the
#'   pre-measure).
#' @param protocol A \code{data.frame} by \code{\link{spiro_protocol}}
#'   containing the test protocol. This is automatically guessed by default.
#'
#' @return A \code{data.frame} of the class \code{spiro_*} with the interpolated
#'   spirometric data, the corresponding load and (if supplied) additional heart
#'   rate data.
#'
#'   The attribute \code{"protocol"} provides additional information
#'   on the underlying testing protocol. The attribute \code{"info"} contains
#'   additional meta-data from the original raw file.
#'
#' @examples
#' # get example file
#' file <- spiro_example("zan_gxt")
#'
#' spiro(file)
#'
#' # manually supply a test protocol
#' spiro(file, protocol = spiro_protocol_gxt(step.count = 9, step.start = 2.5,
#'   step.increment = 0.5, step.duration = 300))
#'
#' @export

spiro <- function(file,
                  device = NULL,
                  weight = NULL,
                  hr_file = NULL,
                  hr_offset = 0,
                  protocol = NULL) {

  dt_imported <- spiro_import(file, device = device)
  if (!is.null(protocol)) {
    ptcl <- protocol
  } else if (all(dt_imported$velocity == 0) | all(is.na(dt_imported$velocity))){
    ptcl <- NA
  } else {
    ptcl <- guess_protocol(dt_imported)
  }
  dt_ipol <- spiro_interpolate(dt_imported)
  dt_ptcl <- apply_protocol(data = dt_ipol, protocol = ptcl)
  dt_out <- spiro_add(data = dt_ptcl, weight = weight)
  if (!is.null(hr_file)) dt_out <- hr_add(data = dt_out,
                                          hr_file= hr_file,
                                          hr_offset = hr_offset)
  if (all(is.na(ptcl))) testtype <- NA
  else testtype <- ptcl$testtype
  class(dt_out) <- c(switch(testtype,
                       constant = "spiro_clt",
                       increment = "spiro_gxt",
                       ramp = "spiro_rmp",
                       NULL),
                     "spiro",
                     "data.frame")
  dt_out
}

