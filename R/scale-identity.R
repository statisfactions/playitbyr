##' Use values without scaling
##'
##' These functions can be added onto a \code{sonify} object to
##' indicate that a given parameter is not to be scaled; the values
##' given in the input data should be take as the given sound values.
##'
##' @param \dots Ignored.
##' 
##' @rdname scale_identity
##' @export
##' 
scale_time_identity <- function(...)
  sonscaling(time =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_pitch_identity <- function(...) 
  sonscaling(pitch =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_dur_identity <- function(...)
  sonscaling(dur =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_vol_identity <- function(...)
  sonscaling(vol =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_pan_identity <- function(...)
  sonscaling(pan =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_tempo_identity <- function(...)
  sonscaling(tempo =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_attkp_identity <- function(...)
  sonscaling(attkp =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_decayp_identity <- function(...)
  sonscaling(decayp =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_mod_identity <- function(...)
  sonscaling(mod =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))

##' @rdname scale_identity
##' @export
scale_indx_identity <- function(...)
  sonscaling(indx =  list(limits = NULL, soundlimits = c(0, 0), function(x, limits, soundlimits) x))
