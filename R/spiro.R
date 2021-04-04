#' Process raw data from metabolic carts/spiroergometric measures
#'
#' \code{spiro} wraps multiple functions to import, process and amplify raw data
#' from metabolic carts into a \code{data.frame}.
#'
#' This function performs multiple operations on raw data from metabolic carts.
#' It imports the raw data from a file, which might be complemented by an
#' additional \code{*tcx} file with heart rate data.
#'
#' Breath-by-breath data is linearly interpolated to get data points for every
#' second. Per default, based on the given velocity data, the underlying testing
#' protocol is guessed and applied to the data. Additional variables of gas
#' exchange are calculated for further analysis.
#'
#' Per default the body weight saved in the file's metadata is used for
#' calculations. It is possible to supply \code{weight} manually to the
#' function.
#'
#' @param file the name of the file which contains the spiroergometry data. The
#'   file  will be found by partial matching of the regular expression within
#'   the working directory (and one level above). Alternatively, \code{file} can
#'   be an absolute or relative path.
#' @param weight Numeric value for participant's body weight, if the default
#'   value saved in \code{file} should be overwritten.
#' @param hr_file the name of a \code{*tcx} file which contains additional heart
#'   rate data. The file  will be found by partial matching of the regular
#'   expression within the working directory (and one level above).
#'   Alternatively, \code{file} can be an absolute or relative path.
#' @param hr_offset An integer, corresponding to the temporal offset of the
#'   heart-rate file. By default the start of the heart rate measurement is
#'   linked to the first load in the test protocol (i.e. the end of the
#'   pre-measure).
#' @param protocol A \code{data.frame} by \code{spiro_protocol} containing the
#'   test protocol. This is automatically guessed by default.
#'
#' @return A \code{data.frame} of the class \code{spiro_*} with the interpolated
#'   spirometric data, the corresponding load and (if supplied) additional heart
#'   rate data.
#'   The attribute \code{"protocol"} provides additional information
#'   on the underlying testing protocol. The attribute \code{"info"} contains
#'   additional meta-data from the original raw file.
#'
#' @export

spiro <- function(file,
                  weight = NULL,
                  hr_file = NULL,
                  hr_offset = 0,
                  protocol = NULL) {

  dt_imported <- spiro_import(file)
  if (is.null(protocol)) ptcl <- guess_protocol(dt_imported)
  else ptcl <- protocol
  dt_ipol <- spiro_interpolate(dt_imported)
  dt_out <- spiro_add(data = apply_protocol(data = dt_ipol, protocol = ptcl),
                      weight = weight)
  if (!is.null(hr_file)) dt_out <- hr_add(data = dt_out,
                                          hr_file= hr_file,
                                          offset = hr_offset)
  class(dt_out) <- c(switch(ptcl$testtype,
                            constant = "spiro_clt",
                            increment = "spiro_gxt",
                            ramp = "spiro_rmp",
                            NULL),
                     "spiro",
                     "data.frame")
  dt_out
}
