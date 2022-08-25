#' Printing spiro data frames
#'
#' Printing method for spiro objects that rounds output to two decimals.
#'
#' @param x A \code{data.frame} of the class \code{spiro} to be printed.
#' @param digits An integer giving the number of decimals to be rounded to.
#' @param ... Passing of additional arguments to \code{print.data.frame()}.
#'
#' @return The function prints its argument and returns it invisibly.
#'
#' @examples
#' # Get example data
#' s <- spiro(spiro_example("zan_gxt"))
#'
#' out <- print(s)
#' head(out)
#' @export
print.spiro <- function(x, digits = 2, ...) {
  x <- round(x, digits = digits)
  NextMethod(x, ...)
}
