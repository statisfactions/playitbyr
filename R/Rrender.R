.createNote <- function(noterow, samp.rate) {
  ## Returns a matrix with 
  ## a note with specified start, pitch, duration, volume, and pan
  
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

octToFreq <- function(oct) {
  ## Converts "oct" notation to an actual frequency
  440*2^(oct-8.75)
}

render.audio <- function(x) {
  notes <- .dfNotes(x)
  samp.rate <- 10000 ## TODO: need to have this as an option

  ## Calculate total number of samples and create data.frame
  ## I add on "nrow(notes)" to the total as a fudge factor
  total <- max(notes$start + notes$dur) * samp.rate + nrow(notes) 
  out <- matrix(data=0, ncol = total, nrow = 2)

  for(i in 1:nrow(notes)) {
    ## Loop to generate each note and put it into the "out" matrix
    curNote <- .createNote(notes[i,], samp.rate)
    out[, curNote$start:curNote$end] <- out[, curNote$start:curNote$end] + curNote$note
  }

  ## Rescale matrix
  out <- linear.scale(out, -1, 1)
  outWave <- as.audioSample(out, samp.rate)
  assign(".LastRendering", outWave, pos=".GlobalEnv")
  return(outWave)
}


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

getPlayer <- function() getOption("player")

setPlayer <- function(newPlayer) options(player = newPlayer)

playLastRendering <- function()  playAudioRendering(.LastRendering)

saveLastRendering <- function(filename) save.wave(.LastRendering, filename)

