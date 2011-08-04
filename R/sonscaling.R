sonscaling <- function(time = list(0, 5, linear.scale),
                       pitch = list(8, 9, linear.scale),
                       dur = list(0.25, 4, linear.scale),
                       vol = list(0.2, 1, linear.scale),
                       pan= list(0, 1, linear.scale),
                       tempo = list(120, 240, linear.scale),
                       timbre = NULL) {
  ##
  ##The sonscaling for a sound parameter is a list with three elements: min, max, and function
  ##
  ##pitch: specified in csound oct notation, with 8.00 as middle C
  ##tempo: specified in proportional relation to total length=1 then multiplied, by default
  ##dur: in relation to 1 "beat", where a "beat" is (total length)/nrow(data)  
  ##vol: specified in relation to loudest sound = 1
  ##timbre: this argument is rendering-specific; there are different ranges of timbre available for
  ##        different renderings. For MIDI notes, just the general MIDI specification
  sc <- list(time, pitch, dur, vol, pan, tempo, timbre)
  names(sc) <- names(formals())
  givens <- names(as.list(match.call())[-1])
  for(i in names(sc)) {
    if(!is.null(sc[[i]])) {
      attr(sc[[i]], "default") <- !(i %in% givens)
      names(sc[[i]]) <- c("min", "max", "scaling.function")
    }
  }

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
  
  class(sc) <- "sonscaling"
  sc
}
