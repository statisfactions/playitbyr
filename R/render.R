render <- function(x, ...) UseMethod("render")
## All render methods should render the sonify object
## to an audioSample object.

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

rendering <- function(x) {
  ## TODO add to namespace
  ## TODO add to render.Rd
  ## This function exists ENTIRELY for changing rendering
  ## in interactive use via '+.sonify';
  ## all it does is check the validitiy of its string argument
  ## and give it the class "sonrendering"
  ## (so that '+.sonify' knows what to do with it)
  
  .checkRendering(x)
  class(x) <- "sonrendering"
  x
}
