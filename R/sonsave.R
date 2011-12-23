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
  if(what$opts$rendering == "csound")
    print(what, file = where, ...)
  if(what$opts$rendering == "audio")
    save.wave(create_audioSample(what), where)
}
