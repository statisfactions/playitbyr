##' Use values without scaling
##'
##' These functions can be added onto a \code{sonify} object to
##' indicate that a given parameter is not to be scaled; the values
##' given in the input data should be take as the given sound values.
##'
##' @param chromatic Should pitches be rounded to chromatic
##' values? Default \code{TRUE}
##' @param \dots Ignored.
##' 
##' @rdname scale_identity
##' @export
scale_time_identity <- function(...)
  sonscaling("time" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_pitch_identity <- function(chromatic = FALSE, ...) {
  if(chromatic)
    return(sonscaling("pitch" = list(0, 0, function(x, min, max) round_chromatic(x))))
  else return(sonscaling("pitch" =  list(0, 0, function(x, min, max) x)))
}

##' @rdname scale_identity
##' @export
scale_dur_identity <- function(...)
  sonscaling("dur" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_vol_identity <- function(...)
  sonscaling("vol" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_pan_identity <- function(...)
  sonscaling("pan" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_tempo_identity <- function(...)
  sonscaling("tempo" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_attkp_identity <- function(...)
  sonscaling("attkp" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_decayp_identity <- function(...)
  sonscaling("decayp" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_mod_identity <- function(...)
  sonscaling("mod" =  list(0, 0, function(x, min, max) x))

##' @rdname scale_identity
##' @export
scale_indx_identity <- function(...)
  sonscaling("indx" =  list(0, 0, function(x, min, max) x))
