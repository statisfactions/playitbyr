##' Internal functions to generate sound from a \code{score} object.
##' 
##' These functions are not intended to be called directly by the
##' user.  \code{render} is a generic that takes a \code{sonify}
##' object and determines which method to call based on the class of
##' the object; play_audioSample plays the resulting
##' \code{audioSample} object.
##' 
##' \code{render.audio} (the only currently available rendering
##' method), calls \code{.createNoteAudio} for every note in the data
##' frame returned by \code{.getScore}. Finally,
##' \code{play_audioSample} is called to produce sound.
##'
##' @note This file is currently under heavy development and will
##' be changing drastically in the development version.
##' @name internalrender
##' @rdname internalrender
##' @aliases render.audio .createNoteAudio

##' @return \code{render.audio} returns an \code{audioSample} object
##' (from the \code{audio} package).
##' 
##' \code{.createNoteAudio} creates each individual note one by one
##' for each row returned by \code{.getNotes}.
##' 
##' \code{play_audioSample} is called for its side effect, to
##' produce the sound of the sonification.
##' @keywords internal
##' @method render audio
##' @export
##' @param x A \code{score} object created by \code{\link{.getScore}}
##' @param \dots Currently ignored.

render.audio <- function(x, audioSample=FALSE, ...) {
  
  samp.rate <- 10000 ## TODO: need to have this as an option

  ## Calculate total number of samples and create matrix based on
  ## first layer.  I add on a quarter-second onto the total length in
  ## seconds, passed as the "length" attribute of the score object,
  ## just to make absolutely sure I have enough space for the
  ## sonification.
  total <- (attributes(x)$length + 0.25) * samp.rate
  out <- matrix(data=0, ncol = total, nrow = 2)

  for(i in 1:length(x)) {
    out <- audio_layer(x[[i]], out, samp.rate)
  }
  
  ## Rescale matrix
  out <- linear.scale(out, -1, 1)
  outWave <- as.audioSample(out, samp.rate)
  assign(".LastRendering", outWave, pos=".GlobalEnv")
  
  if(audioSample)
    return(outWave)
  else
    return(NULL)
}

##' @rdname internalrender
##' @param sonlayerscore An element of the score list--the score
##' produced for a specific layer. The class of this determines the
##' shape to be rendered
##' @param out A matrix containing the rendered output of any
##' previously rendered sonlayers, or a blank matrix large enough to
##' contain the output of this one.
audio_layer <- function(sonlayerscore, out, samp.rate, ...) {
  ## Generic to render a shape
  UseMethod("audio_layer")
}


##' @rdname internalrender
##' @method audio_layer notes
audio_layer.notes <- function(sonlayerscore, out, samp.rate, ...) {
  for(j in 1:nrow(sonlayerscore)) {
    ## Loop to generate each note and put it into the "out" matrix
    curNote <- .createNoteAudio(sonlayerscore[j,], samp.rate)
    out[, curNote$start:curNote$end] <- out[, curNote$start:curNote$end] + curNote$note
  }
  return(out)
}
  

##' @rdname internalrender
##' @param samp.rate The sampling rate, in Hertz
##' @param noterow A row of the \code{data.frame} returned by
##' \code{.getScore}, spoon-fed to \code{.createNoteAudio} one by one
##' by \code{render.audio}
.createNoteAudio <- function(noterow, samp.rate) {
  ## Returns a list with the start and end time of the note, and a
  ## matrix with a rendered with specified start, pitch, duration,
  ## volume, and pan which can be coerced into an audioSample object.
  
  ## "noterow" is intended to be what is returned by a row from funciton df.notes
  start <- round(noterow$start * samp.rate)+1
  n <- round(noterow$dur * samp.rate)
  end <- start + n-1
  note <- matrix(data=0, ncol = n, nrow = 2)
  freq <- octToFreq(noterow$pitch)

  ## Create and trim waveform to last non-zerocrossing
  ## (this avoids clipping)
  waveform <- sin(2 * pi * freq * 0:(n-1) / samp.rate) * noterow$vol 
  less0 <- waveform < 0
  crossing <- c(less0[1:(n-1)] != less0[2:n], FALSE)
  if(any(less0 & crossing)) {
    to0 <- max(which(less0 & crossing)):n
    waveform[to0] <- 0
  }

  ## Multiply waveform by pan and shunt to each speaker
  note[1,] <- waveform * noterow$pan
  note[2,] <- waveform * (1 - noterow$pan)

  return(list(start=start, end=end, note=note))
}  


