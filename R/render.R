##' Render a sonification score to sound
##'
##' \code{render} is a generic which takes a \code{score} object
##' created by \code{\link{.getScore}} and renders it to sound.
##'
##' @param x A \code{score} object created by \code{\link{.getScore}}
##' @param opts the options of the original sonify object. 
##' @param file the file to save the rendering to. The
##' default,\code{""}, plays the sonification in real time.
##' @param \dots Arguments to pass to the specific render method.
##' @return The length of the resulting sonification
##'
##' @keywords internal
render <- function(x, opts, file = "", ...) {
  render.csound(x, opts, file, ...)
  invisible(attr(x, "length"))
}
