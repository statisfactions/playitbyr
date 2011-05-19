## order of operations to get from a sonify object with only one layer to MIDI-rendered sound:

## 1. based on $mapping and $scales, generate a standardized data.frame in
##    somehing like a CSound score that ANY rendering engine should handle
## 2. Apply rendering-specific methods to realize the generated score.
## So let's do this single-layer case in MIDI in the case where pitch is the only relevant
## parameter. And not worrying about microtones.

setMIDIPlayer <- function(x) {
  ## Path to midi Player

  options(MIDI = x)

}

getMIDIPlayer <- function() getOption("MIDI")

setCSVMIDI <- function(x="csvmidi") {
  ## Path to csvmidi executable

  options(csvmidi = x)
}

getCSVMIDI <- function() getOption("csvmidi")

MIDI <- function(title="R-created MIDI", bpm=60){
  ## Creates empty "MIDI" object that tracks can be inserted into

  options(scipen=7) ## avoids writing anything here in scientific notation

  ## Create new data.frame and add headers
  newdf <- data.frame(matrix(nrow=7,ncol=7))
  names(newdf) <- c("tracknum", "time", "type","channel", "note", "velocity", "tempoonly")
  newdf[1,] <- c(0,0, "Header", 1, 1, 480, NA)
  newdf[2,] <- c(1,0, "Start_track", NA, NA, NA, NA)
  newdf[3,] <- c(1,0, "Title_t", title, NA, NA, NA)
  newdf[4,] <- c(1,0, "Time_signature", 4, 2, 24, 8)
  newdf[5,] <- c(1,0, "Tempo", 60000000/bpm, NA,NA,NA)
  newdf[6,] <- c(1,0, "End_track", NA, NA, NA, NA)  
  newdf[7,] <- c(0,0, "End_of_file", NA, NA, NA, NA)

  options(scipen=0) ## reset option

  ## Set class and attributes
  class(newdf) <- c("MIDI", "data.frame")
  attributes(newdf)$istrack <- FALSE
  attributes(newdf)$percussion <- FALSE
  attributes(newdf)$maxtrack <- 0
  attributes(newdf)$maxchannel <- 0
  newdf
}

octToMIDINote <- function(oct) {
  ## This is rounded...evenually it be nice to figure out microtonal
  round(60+ (oct-8)*12)
}  

track <- function(df, percussion = FALSE) {
  beat <- df$start
  durs <- df$dur
  notes <- octToMIDINote(df$pitch)
  veloc <- df$vol * 127
  program <- df$timbre
  tracknum <- rep(NA,length(beat))
  score <- data.frame(tracknum)
  score$starttime <- beat
  score$channel <- NA
  score$note <- notes
  score$velocity <- veloc
  score$endtime <- score$starttime+durs

  ## Change times to MIDI quarter note
  score$starttime <- score$starttime * 480
  score$endtime <- score$endtime * 480

  ons <- score[,setdiff(names(score), "endtime")]
  ons$time <- ons$starttime
  ons$type <- "Note_on_c"
  ons <- ons[, c("tracknum", "time", "type","channel", "note", "velocity")]

  offs <- score[, setdiff(names(score), "starttime")]
  offs$time <- offs$endtime
  offs$type <- "Note_off_c"
  offs$velocity <- 0
  offs <- offs[, c("tracknum", "time", "type","channel", "note", "velocity")]

  all <- rbind(ons, offs)
  all <- all[order(all$time, all$type),]
  all$tempoonly <- NA

  begintrack <- c(NA, 0, "Start_track", NA, NA, NA, NA)
  chooseinstrument <- c(NA, 0, "Program_c", NA, program, NA,NA)
  endtrack <- c(NA, max(all$time), "End_track", NA, NA, NA, NA)
  fulltrack <- rbind(begintrack, chooseinstrument, all, endtrack)

  attributes(fulltrack)$istrack <- TRUE
  attributes(fulltrack)$percussion <- percussion
  fulltrack
}

tempo <- function(bpm=60) {
  class(bpm) <- c("MIDI", "numeric")
  attributes(bpm)$istrack <- FALSE
  bpm
}

addTrack <- function(MIDIOld, MIDITrack, ...) {
  if(attributes(MIDITrack)$istrack){
    ## Add a new MIDI track (first argument) into an existing one (second)
    ## Channel numbering starts at 11 to avoid any confusion with percussion tracks
    ## which in MIDI are always channel 10.
    if(attributes(MIDIOld)$percussion) {
      channelnum <- 10
    } else {
      channelnum <- attributes(MIDIOld)$maxchannel + 1
      tracknum <- attributes(MIDIOld)$maxtrack + 1
    }
    MIDIOld[1,5] <- as.numeric(MIDIOld[1,5])+1
    MIDITrack$tracknum <- tracknum
    MIDITrack$channel <- channelnum
    MIDINew <- rbind(MIDIOld[-nrow(MIDIOld),], MIDITrack, MIDIOld[nrow(MIDIOld),])
    ##
    class(MIDINew) <- c("MIDI", "data.frame")
    attributes(MIDINew)$istrack <- FALSE
    attributes(MIDINew)$percussion <- FALSE
    attributes(MIDINew)$maxtrack <- attributes(MIDIOld)$maxchannel + 1
    attributes(MIDINew)$maxchannel <- attributes(MIDIOld)$maxtrack + 1
  } else {
    MIDINew <- MIDIOld
    MIDINew[5,4] <- 60000000/MIDITrack
  }    
  MIDINew
}

render.MIDI <- function(x) {
  if(is.null(getMIDIPlayer())) stop("No MIDI player specified. Please set with getMIDIPlayer().")

  if(any(sapply(1:length(y$sonlayers), function(x) getMappings(y, x)$pan) != 0.5))
    warning("Pan parameter not currently supported for MIDI rendering and will be ignored.")
  df <- df.notes(x)
  tracklist <- lapply(levels(df$sonlayer), function(x) track(df[df$sonlayer %in% x,]))
  x <- MIDI()
  for(i in 1:length(tracklist))
    x <- addTrack(x, tracklist[[i]])
  outfile <-tempfile()
  write.table(x, file=paste(outfile,"csv",sep="."), quote=F, sep=",", row.names=F, col.names=F, na="")
  system(paste(getCSVMIDI(), paste(outfile,"csv",sep="."), paste(outfile,"mid",sep=".")))
  system(paste(getMIDIPlayer(),paste(outfile,"mid",sep=".")), wait=FALSE)
  unlink(outfile)
}




