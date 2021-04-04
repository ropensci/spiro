#' Get path to spiro example
#'
#' \code{spiro_example} return the file path for example data files within the
#' \code{spiro} package.
#'
#' @param file Name of the file.
#'
#' @export
spiro_example <- function(file) {
  system.file("extdata", file, package = "spiro", mustWork = TRUE)
}
