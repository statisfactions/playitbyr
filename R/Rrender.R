## So there are many complications natively rendering the
## list of instructions included in the output of df.notes
## I need to GENERATE each note, then I need to merge a whole bunch of notes
## together, then I need to normalize so that I'm not out of amplitude.
## Also, I should clip the end of each note to the nearest "0" cutoff point.
## The most straightforward rendering strategy I can think of:


## start: 0s everywhere else besides the exact placement of notes
## pitch: convert from oct to freq, help of csound manual and previous code
## duration: seconds * samp.rate, then cut off at last 0
## volume, pan: each channel multiplies the sine wave by volume * pan

## II. Add all these matrices together, then normalize, then create
## audioSample

all

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

render.audio <- function(s) {
  notes <- .dfNotes(s)
  samp.rate <- 10000 ## FIXME: need to have this as an option

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
  outWave <- as.audioSample(out)
  save.wave(outWave, "foo.wav")
  assign(".LastRendering", outWave, pos=".GlobalEnv")
  system("aplay foo.wav")  
  
}



