##' Render \code{sonify} objects to \code{audioSample}
##' 
##' These functions provide the ability to render sonifications to a
##' \code{audioSample} object from the \pkg{audio} package, and save to a file.
##' \code{\link{print.sonify}} calls both these functions in succession.
##' 
##' 
##' @aliases render playAudioRendering
##' @param x A \code{sonify} object to be rendered.
##' @param ... Other arguments to be passed to the individual rendering option.
##' @param audioSamp An \code{audioSample} object, already rendered and ready
##' to be played or saved to file.
##' @return
##' 
##' \code{render.audio} returns an \code{audioSample} object (from the
##' \code{audio} package) of the sonification.
##' 
##' \code{playAudioRendering} is called for its side effect, to produce the
##' sound. It is just a wrapper for \code{\link[audio]{play}} on Windows and OS
##' X; on Linux systems it creates a temporary file and plays it with an
##' external \code{*.wav} file player (see \code{\link{setPlayer}}).
##' @seealso \code{\link[audio]{save.wave}} for saving \code{audioSample}
##' objects to file; \code{\link{setPlayer}} for getting and setting the method
##' of actually playing the rendering; and \code{\link{sonify}} for general
##' details.
##' @examples
##' 
##' x <- sonify(iris) + sonaes(Petal.Length, Petal.Width)
##' out <- render(x)
##' \dontrun{
##' playAudioRendering(out)
##' ## This is the same as just printing the object:
##' x
##' 
##' ## But you can also save it to file using the audio package:
##' save.wave(out, "myfile.wav")
##' }
##'
##' @rdname render
##' @export
render <- function(x, ...) UseMethod("render")
## All render methods should render the sonify object
## to an audioSample object.

##' @rdname render
##' @export
playAudioRendering <- function(audioSamp) {
  ## Plays the file either using audio::play
  ## or by writing to a tempfile and playing it
  if(is.null(getPlayer())) {
    stop("Please set the wave file player you want to use with setPlayer().
Once you've done so, you can play the last rendering with playLastRendering().

Though you can't play it yet, you can save it to file with
saveLastRendering('path/to/file.wav').")
  } else if(getPlayer() == "audio::play") {
    play(audioSamp)
  } else {
    player <- getPlayer()
    file <- paste(tempfile(), ".wav", sep="")
    save.wave(audioSamp, file)
    system2(player, file)
    unlink(file)
  }
}

