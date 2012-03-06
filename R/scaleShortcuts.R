##' Shortcut functions to set linear_scales for \code{sonify} objects
##' 
##' These shortcut functions (with the notation \code{scale_}\kbd{[data
##' parameter]}\code{_}\kbd{[scaling function]}) are intended to be added onto
##' a \code{sonify} object to specify how \kbd{[data parameter]} from the data
##' (a column of the data) is to be mapped to sonic parameters.
##' 
##' 
##' The syntax of specifying a full \code{sonify} object can be rather verbose.
##' Inspired by the \pkg{ggplot2} package, \pkg{playitbyr} provides shortcut
##' functions to make the process easier and more intuitive.
##' 
##' These functions are intended to be added onto a pre-existing sonify object,
##' as shown in the examples.
##'
##' @rdname scaleShortcuts
##' @aliases scaleShortcuts
##' @param min The desired minimum value, a \code{numeric} of length 1
##' @param max The desired maximum value, a \code{numeric} of length 1
##' @param dmin The data value to be lined up with the \code{min}
##' argument. This is useful for when you want to specify a fixed
##' scale
##' @param dmax The data value to be lined up with the \code{max}
##' argument. This is useful for when you want to specify a fixed
##' scale
##' @param chromatic For pitches, should they be rounded to chromatic
##' values? Default \code{TRUE}
##' 
##' @return A \code{sonscaling} object, to be used in \code{sonify} or added
##' onto a \code{sonify} object.
##' @seealso \code{\link{sonscaling}} for the full syntax and range of
##' possibilities for specifying scaling of \code{sonify} objects;
##' \code{\link{linear_scale}} for the pre-defined linear scaling function
##' which this incorporates; \code{\link{+.sonify}} for the addition onto
##' \code{sonify} objects.
##' @examples
##' 
##' ## A verbose way of specifying a sonify object,
##' ## without using the shortcuts
##' x <- sonify(data=iris, 
##'      sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75),
##'      scales = sonscaling(
##'        time = list(min=0, max=3,
##'         scaling.function = linear_scale),
##'        pitch = list(min=3, max=13,
##'         scaling.function = linear_scale)),
##'      sonlayers = shape_scatter())
##' summary(x)
##' \dontrun{x}
##' 
##' ## An equivalent and much more readable way, with
##' ## the shortcut functions
##' y <- sonify(iris, sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75))
##' y <- y + shape_scatter()
##' y <- y + scale_time_continuous(0, 3)
##' y <- y + scale_pitch_continuous(3, 13)
##' summary(y)
##' \dontrun{y}
##' 
##' ## You can also change a mapping incrementally.
##' ## New parameters overwrite old ones, so now
##' ## the sonification is stretched to 12 seconds
##' ## long instead of 3, and the pitch is mapped to
##' ## a much narrower range, the octave below tuning A (440 Hz)
##' y <- y + scale_time_continuous(0, 12) + scale_pitch_continuous(7, 8)
##' summary(y)
##' \dontrun{y}
##'
##' @export
##################################################
## LINEAR SCALES
##################################################
scale_time_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("time", min, max, dmin, dmax)


##' @rdname scaleShortcuts
##' @export
scale_pitch_continuous <- function(min, max, dmin = NULL, dmax = NULL, chromatic = TRUE) {
  if(chromatic)
    return(linear_fixed_scale_chromatic(min, max, dmin, dmax))
  else return(linear_fixed_scale("pitch", min, max, dmin, dmax))
}

##' @rdname scaleShortcuts
##' @export
scale_dur_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("dur", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_vol_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("vol", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_pan_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("pan", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_tempo_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("tempo", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_attkp_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("attkp", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_decayp_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("decayp", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_mod_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("mod", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_indx_continuous <- function(min, max, dmin = NULL, dmax = NULL) linear_fixed_scale("indx", min, max, dmin, dmax)

##################################################
## EXPONENTIAL SCALES
##################################################
##' @rdname scaleShortcuts
##' @export
scale_time_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("time", min, max, dmin, dmax)


##' @rdname scaleShortcuts
##' @export
scale_pitch_exp <- function(min, max, dmin = NULL, dmax = NULL, chromatic = TRUE) {
  if(chromatic)
    return(exp_fixed_scale_chromatic(min, max, dmin, dmax))
  else return(exp_fixed_scale("pitch", min, max, dmin, dmax))
}

##' @rdname scaleShortcuts
##' @export
scale_dur_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("dur", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_vol_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("vol", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_pan_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("pan", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_tempo_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("tempo", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_attkp_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("attkp", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_decayp_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("decayp", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_mod_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("mod", min, max, dmin, dmax)

##' @rdname scaleShortcuts
##' @export
scale_indx_exp <- function(min, max, dmin = NULL, dmax = NULL) exp_fixed_scale("indx", min, max, dmin, dmax)

##################################################
## IDENTITY SCALES
##################################################
##' @rdname scaleShortcuts
##' @export
scale_time_identity <- function()
  sonscaling("time" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_pitch_identity <- function(chromatic = FALSE) {
  if(chromatic)
    return(sonscaling("pitch" = list(0, 0, function(x, min, max) round_chromatic(x))))
  else return(sonscaling("pitch" =  list(0, 0, function(x, min, max) x)))
}

##' @rdname scaleShortcuts
##' @export
scale_dur_identity <- function()
  sonscaling("dur" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_vol_identity <- function()
  sonscaling("vol" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_pan_identity <- function()
  sonscaling("pan" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_tempo_identity <- function()
  sonscaling("tempo" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_attkp_identity <- function()
  sonscaling("attkp" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_decayp_identity <- function()
  sonscaling("decayp" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_mod_identity <- function()
  sonscaling("mod" =  list(0, 0, function(x, min, max) x))

##' @rdname scaleShortcuts
##' @export
scale_indx_identity <- function()
  sonscaling("indx" =  list(0, 0, function(x, min, max) x))

fixie <- function(column, min, max) {
  dmin <- 1
  dmax <- 3
  exp_scale(c(dmin, dmax, column), min, max)[-(1:2)]
}

exp_fixed_scale <- function(param, min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, exp_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    exfixie <- fixie
    body(exfixie)[[2]][[3]] <- dmin
    body(exfixie)[[3]][[3]] <- dmax
    out <- sonscaling("start" = list(min, max, exfixie))
  }
  names(out) <- param
  out
}

linear_fixed_scale <- function(param, min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, linear_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    linfixie <- fixie
    body(linfixie)[[2]][[3]] <- dmin
    body(linfixie)[[3]][[3]] <- dmax
    body(linfixie)[[4]][[2]][[1]] <- substitute(linear_scale)
    out <- sonscaling("start" = list(min, max, linfixie))
  }
  names(out) <- param
  out
}


## returns vector rounded to the nearest twelfth for use with
## chromatic scales This whole section is a horrible cheap hack and I
## feel very sorry for anyone trying to figure this out and apologize
## humbly. Really, I should be able to pass arbitrary parameters to
## scaling functions is the real lesson here.
round_chromatic <- function(x) round(x*12)/12

chromfixie <- function(column, min, max) {
  dmin <- 1
  dmax <- 3
  exp_scale(c(dmin, dmax, column), min, max)[-(1:2)] -> out
  round_chromatic(out)
}

linear_fixed_scale_chromatic <- function(min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, linear_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    linfixie <- chromfixie
    body(linfixie)[[2]][[3]] <- dmin
    body(linfixie)[[3]][[3]] <- dmax
    body(linfixie)[[4]][[3]][[2]][[1]] <- substitute(linear_scale)
    out <- sonscaling("start" = list(min, max, linfixie))
  }
  names(out) <- "pitch"
  out
}

exp_fixed_scale_chromatic<- function(min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, exp_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    exfixie <- chromfixie
    body(exfixie)[[2]][[3]] <- dmin
    body(exfixie)[[3]][[3]] <- dmax
    out <- sonscaling("start" = list(min, max, exfixie))
  }
  names(out) <- "pitch"
  out
}

