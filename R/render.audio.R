##' Rendering sound using the audio package
##' 
##' These functions are not intended to be called directly by the
##' user.  \code{render} is a generic that takes a \code{sonify}
##' object and determines which method to call based on the class of
##' the object; play_audioSample plays the resulting
##' \code{audioSample} object.
##' 
##' \code{render.audio} calls \code{.createNoteAudio} for every note
##' in the data frame returned by \code{.getScore}. Finally,
##' \code{play_audioSample} is called to produce sound.
##'
##' @note This file is currently under heavy development and will
##' be changing drastically in the development version.
##' @name render.audio
##' @rdname render.audio
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
  
  samp.rate <- attributes(x)$render_options$samp.rate
  if(is.null(samp.rate))
    samp.rate <- 10000
  
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

##' @rdname render.audio
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


##' @rdname render.audio
##' @method audio_layer notes
audio_layer.notes <- function(sonlayerscore, out, samp.rate, ...) {
  for(j in 1:nrow(sonlayerscore)) {
    ## Loop to generate each note and put it into the "out" matrix
    curNote <- .createNoteAudio(sonlayerscore[j,], samp.rate)
    out[, curNote$start:curNote$end] <- out[, curNote$start:curNote$end] + curNote$note
  }
  return(out)
}
  

##' @rdname render.audio
##' @method audio_layer dotplot
audio_layer.dotplot <- function(sonlayerscore, out, samp.rate, ...) {
  ## Returns a list with a matrix with sonified clicks 
  ## which can be coerced into an audioSample object.

  # Make an array of  start times
  starts=0;
  for (i in 1:nrow(sonlayerscore)){ 
    noterow = sonlayerscore[i,];  
    starts[i] <- round(noterow$start * samp.rate)+1 
  }


  # Make a matrix with clicks
  # click = c(1, seq(1,0,by=-1/(44100/1000))^2) ;
  click = c(1,-1);
  click = t(matrix(data=click,nrow=2,ncol=2));
  for (i in 1:length(starts)){
      out[1:2,starts[i]:(starts[i]+1)] = (out[1:2,starts[i]:(starts[i]+1)] + click)/2 ;
  }
  
  plot(out[2,],type="l")
  
  out= out *0.1;
  # No pan for now
  # out[1,] <- waveform * noterow$pan
  # out[2,] <- waveform * (1 - noterow$pan)
  return(out)
}
  
  

##' @rdname internalrender
##' @rdname render.audio
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



###' @rdname internalrender
###' @param samp.rate The sampling rate, in Hertz
###' @param noterow A row of the \code{data.frame} returned by
###' \code{.getScore}, spoon-fed to \code{.createNoteAudio} one by one
###' by \code{render.audio}
#.createToneAudio <- function(notescore, samp.rate, out) {
  ## Returns a list with the start and end time of the tone, and a
  ## matrix with a rendered with specified start, pitch, duration,
  ## volume, and pan which can be coerced into an audioSample object.
  # Make an array of frequencies and start times (ignore space at start)
  # Ignore duration info, we will be sliding between notes 
#  starts=0;
#  freq=0;
#  for (i in 1:nrow(notescore)){	
#  	noterow = notescore[i,];	
#  	starts[i] <- round(noterow$start * samp.rate)+1 
#  	freq[i] <- octToFreq(noterow$pitch)
#  }
#
#  # We have frequencies and start times, now add the final duration. 
#  starts[nrow(start)+1] = starts[nrow(starts)] + noterow$dur; 
#  freq[nrow(freq)+1] = freq[nrow(freq)]; 
#  cat("Length: ");   cat(length(start)); cat("\n");
#
#  carrier = out[1,];
#  waveform= out[1,];
#
#  len = 1;
#  carrier=0;
#  rampend = 1;
#
#  # Make a matrix that represents a carrier frequency ramp
#  for (i in 2:length(starts)){
#  	duration  <- starts[i] - starts[i-1];
#	from 	  <- freq[i-1];
#	to        <- freq[i-1] + (freq[i] - freq[i-1])/2 ;
##	cat(from); cat(" "); cat(to); cat(" ");  cat(freq[i]); cat("\n");
#  	tmp       <- seq(from, to, length.out=duration);
#	cat(rampend); cat(" "); cat(rampend + length(tmp) - 1); cat("\n");  
#    ramp      <- seq(rampend, rampend + length(tmp) - 1 );  
#    ramp      <- ramp %% samp.rate;
#	rampend   <- ramp[length(ramp)];
#    tmp       <- sin(2 * pi * (ramp/samp.rate) * tmp);
#  	waveform  <- c(waveform,tmp);
#  	len       <- len + duration; 
#  }
#  
#  plot(carrier)
#
#  waveform = waveform *0.1;
##  plot(waveform,type="l")
#
#  aud = audioSample(waveform, rate=44100, bits=16, clip = TRUE)
#  save.wave(aud, "filer.wav")
#
#  # Multiply waveform by pan and shunt to each speaker
#  # note[1,] <- waveform * noterow$pan
#  # note[2,] <- waveform * (1 - noterow$pan)
#
#  return(list(start=start, end=end, note=waveform))
#}  


