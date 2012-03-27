##' Save sonifications to an audio file
##'
##' Convenience function to save a \code{sonify} object to an audio
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
  length <- print(what, file = where, ...)
  if(play) {
    createPerformance(i = list(matrix(c(3, 0, length,
                        paste("\"", where, "\"", sep  = "")),
                        nrow = 1)), out = out,
                      orcfile = system.file("orc/playitbyr.orc", package = "playitbyr"))
  }
  invisible(length)
}
