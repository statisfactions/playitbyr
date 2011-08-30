##' Setting scales for \code{sonify} objects
##' 
##' \code{sonscaling()} is invoked in the call to \code{sonify} or added onto
##' it in order to specify precisely how the data parameters are mapped onto
##' sonic parameters. Currently, only continuous scales are supported.
##' 

##' 
##' Each argument of \code{sonscaling} is in the form of a list, where the
##' first element is the minimum value of the sonic parameter, the second is
##' the maximum, and the third is a function that maps the data column onto the
##' range of the sonic parameter. The only such function included with
##' \pkg{playitbyr} right now is \code{\link{linear.scale}} but users can write
##' their own function for this purpose. User-defined scaling functions need to
##' be of the same form as \code{\link{linear.scale}}, i.e. it must have
##' arguments for the input vector \code{x}, and the \code{min} and \code{max}.
##' 
##' @param time The desired start time mapping of all events (in seconds). A
##' list with three elements: the minimum time, maximum time, and the scaling
##' function.
##' @param pitch The desired pitch mapping of all events, represented such that
##' 8 represents middle C, 9 represents the octave above, etc. (This is
##' \href{http://www.csounds.com/manual/html/cpsoct.html}{Csound's \sQuote{oct}
##' notation}. A list with three elements: the minimum pitch, maximum pitch,
##' and the scaling function.)
##' @param dur The desired duration mapping (in seconds). A list with three
##' elements: the minimum duration, the maximum duration, and the scaling
##' function.
##' @param vol The desired volume mapping as a number between 0, silence, and
##' 1, the maximum possible amplitude. A list with three elements: the minimum
##' volume, the maximum volume, and the scaling function.
##' @param pan The desired mapping for balance between left and right stereo
##' channels, where 0 is all the left channel, 0.5 balanced, and 1 all the
##' right. A list with three elements: the minimum pan, the maximum pan, and
##' the scaling function.
##' @param tempo The desired tempo mapping (in beats per minute). A list with
##' three elements: the minimum tempo, the maximum tempo, and the scaling
##' function.
##' @param timbre Ignored. Currently no mappings are supported for this timbre
##' @seealso \code{\link{sonify}} where this is eventually used;
##' \code{\link{sonaes}} for defining the which data columns get mapped onto
##' these sonic parameters; \code{\link{scaleShortcuts}} for easy shortcut
##' functions for common use-cases of scaling; and \code{\link{linear.scale}}
##' for an example of a scaling function.
##' @export
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
