#' Printing spiro data frames
#'
#' Printing method for spiro objects that rounds output to two decimals.
#'
#' @param x A \code{data.frame} of the class \code{spiro} to be printed.
#' @param round An integer giving the number of decimals to be rounded to.
#' @param ... Passing of additional arguments to \code{print.data.frame()}.
#'
#' @export
print.spiro <- function(x, round = 2, ...) {
  x <- round(x, digits = round)
  NextMethod(x, ...)
}
