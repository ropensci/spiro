#' Get path to spiro example
#'
#' \code{spiro_example} returns the file path for example data files within the
#' \code{spiro} package.
#'
#' @param file Name of the file. Either "zan_gxt", "zan_ramp" or "hr_ramp.tcx".
#'
#' @examples
#' spiro_example("zan_gxt")
#' @export
spiro_example <- function(file) {
  system.file("extdata", file, package = "spiro", mustWork = TRUE)
}