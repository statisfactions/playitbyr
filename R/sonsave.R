##' Save sonifications to an audio file
##'
##' Convenience function to save a \code{\link{sonify}} object to an audio
##' (\code{.wav}) file
##'
##' @export
##' @param what A sonify object
##' @param where The path to the desired \code{.wav} output.
##' @param play Play the resulting file after saving?
##' @param out If playing, this gives the output channel. Usually users will want to leave this as \code{"dac"}, the default (which plays to the default audio out).
##' @param \dots other arguments to be passed onto the rendering.
##' @return Returns the filename of the saved file.
sonsave <- function(what, where, play = FALSE, out = "dac", ...) {
  if(where == "")
    stop("'where' cannot be an empty string")
  length <- print(what, file = where, render_real_time = FALSE, play = play, playout = out, ...)

  invisible(length)
}
    
