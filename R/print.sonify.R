##' Print method for \code{sonify} objects
##' 
##' Printing a sonify object renders it to sound (analogously to how
##' printing \code{ggplot} objects renders them to screen in the
##' \href{http://had.co.nz/ggplot2}{ggplot2} package).
##' 
##'
##' @aliases print.sonify
##' @param x, A \code{sonify} object
##' 
##' @param \dots Ignored.
##' @return \code{print.sonify} is called for its side-effect, which is to
##' actually render the object to a sound.
##' 
##' @seealso \code{\link{sonify}} for the creation of these objects
##' @method print sonify
##' @export
print.sonify <- function(x, ...) {
  render(.getScore(x), opts = x$opts, ...)
  return(NULL)
}
