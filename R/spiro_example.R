#' Get path to spiro example
#'
#' \code{spiro_example} returns the file path for example data files within the
#' \code{spiro} package.
#'
#' @param file Name of the file, either "zan_gxt", "zan_ramp" or "hr_ramp.tcx".
#'   Leave the argument empty to get a vector with the paths of all example
#'   files.
#'
#' @return A character vector with the absolute file path of the example
#'   file(s).
#'
#' @examples
#' # get path of a specific example data file
#' spiro_example("zan_gxt")
#'
#' # get all paths of example data files
#' spiro_example()
#' @export
spiro_example <- function(file = NULL) {
  if (is.null(file)) {
    path <- system.file("extdata", package = "spiro")
    out <- list.files(root, full.names = TRUE)
  } else {
    out <- system.file("extdata", file, package = "spiro", mustWork = TRUE)
  }
  out
}
