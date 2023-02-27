#' Get raw data from a metabolic cart file or an imported spiro object
#'
#' \code{spiro_raw()} retrieves cardiopulmonary raw data from various types of
#' metabolic cart files, or from objects previously imported and processed with
#' \code{\link{spiro}}.
#'
#' The default way of importing data into the spiro package is using the
#' \code{\link{spiro}} function. Besides importing this will perform further
#' processing steps such as interpolating of data or calculation of more
#' variables. But in some cases the original raw data may be preferable compared
#' to the processed raw data. \code{spiro_raw} can either retrieve the raw data
#' from an already imported data set or from a new raw data file.
#'
#' @param data Either the absolute or relative path of the file that contains the
#'   gas exchange data, or a data frame of the class \code{spiro}, usually the
#'   output of the \code{\link{spiro}} function.
#' @inheritParams spiro
#'
#' @return A \code{data.frame} with data. The attribute \code{info} contains
#'   addition meta-data retrieved from the original file.
#'
#' @examples
#' # Get example data
#' file <- spiro_example("zan_gxt")
#'
#' # direct import of raw data
#' out <- spiro_raw(file)
#' head(out)
#'
#' # retrieval of raw data from previously processed object
#' s <- spiro(file)
#' out2 <- spiro_raw(s)
#' head(out2)
#' @export
spiro_raw <- function(data, device = NULL, anonymize = TRUE) {
  UseMethod("spiro_raw")
}

#' @describeIn spiro_raw Method for direct import from metabolic cart raw data
#'   file
#' @export
spiro_raw.default <- function(data, device = NULL, anonymize = TRUE) {
  spiro_get(file = data, device = device, anonymize = anonymize)
}

#' @describeIn spiro_raw Method for objects of class \code{spiro}, usually files
#'   previously imported and processed with \code{\link{spiro}}
#' @export
spiro_raw.spiro <- function(data, device = NULL, anonymize = TRUE) {
  # check if already anonymized
  is_anon <- any(colnames(attr(data, "info")) == "id")
  if (isTRUE(anonymize)) {
    # forced anonymization
    if (!is_anon) attr(data, "info") <- spiro_anonymize(attr(data, "info"))
  } else if (isFALSE(anonymize)) {
    # forced de-anonymization (not possible)
    if (is_anon) warning("Cannot deanonymize data.")
  } else {
    warning("'anonymize' argument must be either TRUE or FALSE'")
  }
  if (!is.null(device)) {
    warning(
      paste0("'device' argument in spiro_raw() is ignored when called for an ",
             " spiro object")
    )
  }
  # rewrite meta data attribute
  out <- attr(data, "raw")
  attr(out, "info") <- attr(data, "info")
  out
}
