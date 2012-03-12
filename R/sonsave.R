##' Save sonifications to an audio file
##'
##' Convenience function to save a \code{sonify} object to an audio
##' (\code{.wav}) file
##'
##' @export
##' @param what A sonify object
##' @param where The path to the desired \code{.wav} output
##' @param \dots other arguments to be passed onto the rendering
sonsave <- function(what, where, ...) {
  print(what, file = where, ...)
}
