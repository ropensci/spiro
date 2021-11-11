#' Process raw data from metabolic carts/spiroergometric measures
#'
#' \code{spiro()} wraps multiple functions to import and process raw data from
#' metabolic carts into a \code{data.frame}.
#'
#' This function performs multiple operations on raw data from metabolic carts.
#' It imports the raw data from a file, which might be complemented by an
#' additional \code{.tcx} file with heart rate data. For details of data import
#' and supported file types visit \code{\link{spiro_import}}.
#'
#' Breath-by-breath data is linearly interpolated to get data points for every
#' second. Based on the given load data, the underlying exercise protocol is
#' guessed and applied to the data. If no load data is available or the protocol
#' guess turns wrong, you can manually specify the exercise \code{protocol} by
#' using \code{\link{set_protocol}} or \code{\link{set_protocol_manual}}.
#'
#' Additional variables of gas exchange are calculated for further analysis. Per
#' default the body weight saved in the file's metadata is used for calculating
#' relative measures. It is possible to supply \code{weight} manually to the
#' function, overriding that value.
#'
#' Protocols, heart rate data and weight information can also be given in a
#' piping coding style using the functions \code{\link{add_protocol}},
#' \code{\link{add_hr}} and \code{\link{add_weight}} (see examples).
#'
#' After processing, you may summarize the resulting data frame with
#' \code{\link{spiro_summary}} and \code{\link{spiro_max}}, or plot it with
#' \code{\link{spiro_plot}}.
#'
#' @param file The absolute or relative path of the file that contains the gas
#'   exchange data.
#' @param device A character string, specifying the device for measurement. By
#'   default the device type is guessed by the characteristics of the
#'   \code{file}. This can be overridden by setting the argument to
#'   \code{"cortex"}, \code{"cosmed"} or \code{"zan"}.
#' @param weight Numeric value for participant's body weight, if the default
#'   value saved in the \code{file} should be overridden.
#' @param hr_file The absolute or relative path of a \code{*tcx} file that
#'   contains additional heart rate data.
#' @param hr_offset An integer, corresponding to the temporal offset of the
#'   heart-rate file. By default the start of the heart rate measurement is
#'   linked to the start of the gas exchange measurement.
#' @param protocol A \code{data.frame} by \code{\link{set_protocol}} or
#'   \code{\link{set_protocol_manual}} containing the test protocol. This is
#'   automatically guessed by default.
#'
#' @return A \code{data.frame} of the class \code{spiro_*} with the interpolated
#'   spirometric data, the corresponding load and (if supplied) additional heart
#'   rate data.
#'
#'   The attribute \code{"protocol"} provides additional information on the
#'   underlying testing protocol. The attribute \code{"info"} contains
#'   additional meta-data from the original raw file.
#'
#' @examples
#' # get example file
#' file <- spiro_example("zan_gxt")
#'
#' spiro(file)
#'
#' # import with user-defined test profile
#' spiro(file, protocol = set_protocol(pre(60),steps(300,2,0.4,9,30)))
#'
#' # import with additional heart rate data
#' oxy_file <- spiro_example("zan_ramp")
#' hr_file <- spiro_example("hr_ramp.tcx")
#'
#' spiro(oxy_file, hr_file = hr_file)
#'
#' # use the add_* functions in a pipe
#' spiro(file) |>
#'   add_hr(hr_file = hr_file, hr_offset = 0) |>
#'   add_weight(68.2)
#'
#' @export

spiro <- function(file,
                  device = NULL,
                  weight = NULL,
                  hr_file = NULL,
                  hr_offset = 0,
                  protocol = NULL) {

  # validate inputs
  if (!is.null(weight)) {
    if (!is.numeric(weight)) {
      stop("'weight' must be a numeric value")
    } else if (weight <= 0) {
      stop("'weight' must be greater than 0")
    }
  }

  if (!is.null(hr_file) && !is.numeric(hr_offset)) {
    stop("'hr_offset' must be a numeric value")
  }

  # import the gas exchange raw data
  dt_imported <- spiro_import(file, device = device)

  # find or guess an exercise protocol
  if (!is.null(protocol)) { # use manually specified protocol
    ptcl <- protocol
  } else if (all(dt_imported$load == 0)) { # no protocol available
    ptcl <- NULL
  } else { # guess protocol
    ptcl <- get_protocol(dt_imported)
  }

  # interpolate the data
  dt_ipol <- spiro_interpolate(dt_imported)

  # add a protocol
  dt_ptcl <- add_protocol(data = dt_ipol, protocol = ptcl)

  # add data calculated from body weight
  dt_out <- add_weight(data = dt_ptcl, weight = weight)

  # calculate additional variables
  dt_out$RER <- dt_out$VCO2 / dt_out$VO2
  dt_out <- calo(data = dt_out)

  # Add heart rate if available
  if (!is.null(hr_file)) {
    dt_out <- add_hr(data = dt_out, hr_file= hr_file, hr_offset = hr_offset)
  }

  # save raw data as attribute
  attr(dt_out, "raw") <- dt_imported

  dt_out
}

