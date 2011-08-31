##' Render \code{sonify} objects to \code{audioSample}
##' 
##' These functions provide the ability to render sonifications to a
##' \code{audioSample} object from the \pkg{audio} package, and save to a file.
##' \code{\link{print.sonify}} calls both these functions in succession.
##' 
##' 
##' @aliases create_audioSample play_audioSample
##' @param x A \code{sonify} object to be rendered.
##' @param ... Other arguments to be passed to the individual rendering option.
##' @param play Logical indicating whether the created audioSample should be played immediately
##' @param audioSamp An \code{audioSample} object, already rendered and ready
##' to be played or saved to file.
##' @return
##' 
##' \code{render.audio} returns an \code{audioSample} object (from the
##' \code{audio} package) of the sonification.
##' 
##' \code{play_audioSample} is called for its side effect, to produce the
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
##' out <- create_audioSample(x)
##' \dontrun{
##' play_audioSample(out)
##' ## This is the same as just printing the object:
##' x
##' 
##' ## But you can also save it to file using the audio package:
##' save.wave(out, "myfile.wav")
##' }
##'
##' @rdname create_audioSample
##' @export
create_audioSample <- function(x, play=FALSE) {
  checkSonify(x)
  score <- .getScore(x)
  out <- render(x, audioSample=TRUE)
  
  if(play) play_audioSample(out)
  
  return(out)
}




##' @rdname create_audioSample
##' @export
play_audioSample <- function(audioSamp) {
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

