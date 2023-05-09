#' Printing spiro data frames in a knitr context
#'
#' \code{knit_print.spiro()} provides a method for printing \code{data.frames}
#' from \code{\link{spiro}} within \code{knitr}.
#'
#' Cardiopulmonary exercise testing data imported by \code{\link{spiro}} will
#' often come in large \code{data.frame}s. When knitting R Markdown documents
#' these will normally be printed in full size.
#'
#' This function provides a method for \code{data.frame}s of the class
#' \code{spiro} to limit the number of rows displayed to \code{min} if it
#' exceeds \code{max}. The number of hidden data rows will be printed below the
#' \code{data.frame}.
#'
#' @param x A \code{data.frame} of the class \code{spiro} to be printed.
#' @param min An integer, which sets the number of rows to which \code{x} will
#'   be limited in printing if row number exceed \code{max}.
#' @param max An integer, setting the maximal number of rows to be not cut to
#'   \code{min} in printing.
#' @param digits An integer giving the number of decimals to be rounded to.
#' @param ... Passing of additional arguments to \code{knit_print.default()}.
#'
#' @return The function prints its argument and returns it invisibly.
#'
#' @examples
#' # Get example data
#' s <- spiro(spiro_example("zan_gxt"))
#'
#' knitr::knit_print(s)
#' @export
#' @importFrom knitr knit_print
#' @export

knit_print.spiro <- function(x, min = 10, max = 20, digits = 2, ...) {
  n <- nrow(x)
  # rounding
  x[, 4:ncol(x)] <- round(x[, 4:ncol(x)], digits = digits)
  if (n <= max) {
    NextMethod(x, ...)
  } else {
    x <- x[1:min, ]
    nr <- n - min
    c(NextMethod(x, ...), sprintf("... with %s more rows", nr))
  }
}
