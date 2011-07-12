sonscaling <- function(pitch = NULL, time = NULL, tempo = NULL, dur = NULL, vol = NULL, pan=NULL, timbre = NULL) {
  ##
  ##The sonscaling for a sound parameter is a list with three elements: min, max, and function
  ##
  ##pitch: specified in csound oct notation, with 8.00 as middle C
  ##tempo: specified in proportional relation to total length=1 then multiplied, by default
  ##dur: in relation to 1 "beat", where a "beat" is (total length)/nrow(data)  
  ##vol: specified in relation to loudest sound = 1
  ##timbre: this argument is rendering-specific; there are different ranges of timbre available for
  ##        different renderings. For MIDI notes, just the general MIDI specification

  
  sc <- list(pitch, time, tempo, dur, vol, pan, timbre)
  sc <- lapply(sc, function(x) {
               if(length(x) == 3)
                 names(x) <- c("min", "max", "scaling.function")
               return(x)})
  names(sc) <- c("pitch", "time", "tempo", "dur", "vol", "pan", "timbre")
   if(!is.null(sc$time) && ((sc$time$max < 0) || (sc$time$min < 0)))
    stop("time must be greater than 0.")
  if(!is.null(sc$tempo) && ((sc$tempo$max < 0) || (sc$tempo$min< 0)))
    stop("tempo must be at least 0 (bpm)")
  if(!is.null(sc$dur) && ((sc$dur$max<0) || sc$dur$min<0))
    stop("dur cannot be negative")
  if(!is.null(sc$vol) && (((sc$vol$max<0) || sc$vol$min<0) || (sc$vol$min>1 || sc$vol$max>1)))
    stop("vol must be between 0 and 1.")
  if(!is.null(sc$pan) && (((sc$pan$max<0) || sc$pan$min<0) || (sc$pan$min>1 || sc$pan$max>1)))
    stop("pan must be between 0 and 1.")
 
  class(sc) <- c("sonscaling", "list")
  sc
}

## Convenience functions for specifying scales.
scale_pitch_linear <- function(min, max) sonscaling(pitch=list(min, max, linear.scale))
scale_time_linear <- function(min, max) sonscaling(time=list(min, max, linear.scale))
scale_tempo_linear <- function(min, max) sonscaling(tempo=list(min, max, linear.scale))
scale_dur_linear <- function(min, max) sonscaling(dur=list(min, max, linear.scale))
scale_pan_linear <- function(min, max) sonscaling(pan=list(min, max, linear.scale))
scale_vol_linear <- function(min, max) sonscaling(vol=list(min, max, linear.scale))

linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum

  if(min>max) {
    ## Allow for reversed polarity
    x <- -x
    oldmin <- min
    oldmax <- max
    min <- oldmax
    max <- oldmin
  }
    
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}

