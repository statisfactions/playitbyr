render.audio <- function(x) {
  ## Renders sonify object to audioSample object
  
  notes <- .dfNotes(x)
  samp.rate <- 10000 ## TODO: need to have this as an option

  ## Calculate total number of samples and create data.frame
  ## I add on "nrow(notes)" to the total as a fudge factor
  total <- max(notes$start + notes$dur) * samp.rate + nrow(notes) 
  out <- matrix(data=0, ncol = total, nrow = 2)

  for(i in 1:nrow(notes)) {
    ## Loop to generate each note and put it into the "out" matrix
    curNote <- .createNoteAudio(notes[i,], samp.rate)
    out[, curNote$start:curNote$end] <- out[, curNote$start:curNote$end] + curNote$note
  }

  ## Rescale matrix
  out <- linear.scale(out, -1, 1)
  outWave <- as.audioSample(out, samp.rate)
  assign(".LastRendering", outWave, pos=".GlobalEnv")
  return(outWave)
}

.createNoteAudio <- function(noterow, samp.rate) {
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
