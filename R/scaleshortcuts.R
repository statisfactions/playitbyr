##' Shortcut functions to set linear scales for \code{sonify} objects
##' 
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
##' @rdname scaleshortcuts
##' @aliases scaleShortcuts scale_dur_linear scale_pan_linear
##' scale_pitch_linear scale_tempo_linear scale_time_linear scale_vol_linear
##' @param min The desired minimum value, a \code{numeric} of length 1
##' @param max The desired maximum value, a \code{numeric} of length 1
##' @return A \code{sonscaling} object, to be used in \code{sonify} or added
##' onto a \code{sonify} object.
##' @seealso \code{\link{sonscaling}} for the full syntax and range of
##' possibilities for specifying scaling of \code{sonify} objects;
##' \code{\link{linear.scale}} for the pre-defined linear scaling function
##' which this incorporates; \code{\link{+.sonify}} for the addition onto
##' \code{sonify} objects.
##' @examples
##' 
##' ## A verbose way of specifying a sonify object,
##' ## without using the shortcuts
##' x <- sonify(data=iris, 
##'      sonaes(Petal.Width, Petal.Length, dur=5, vol=0.75),
##'      scales = sonscaling(
##'        time = list(min=0, max=3,
##'         scaling.function = linear.scale),
##'        pitch = list(min=3, max=13,
##'         scaling.function = linear.scale)))
##' summary(x)
##' \dontrun{x}
##' 
##' ## An equivalent and much more compact way, with
##' ## the shortcut functions
##' y <- sonify(iris, sonaes(Petal.Width, Petal.Length, dur=5, vol=0.75))
##' y <- y + scale_time_linear(0, 3)
##' y <- y + scale_pitch_linear(3, 13)
##' summary(y)
##' \dontrun{y}
##' 
##' ## You can also change a mapping incrementally.
##' ## New parameters overwrite old ones, so now
##' ## the sonification is stretched to 12 seconds
##' ## long instead of 3, and the pitch is mapped to
##' ## a much narrower range, the octave below tuning A (440 Hz)
##' y <- y + scale_time_linear(0, 12) + scale_pitch_linear(7, 8)
##' summary(y)
##' \dontrun{y}
##'
##' @export
scale_time_linear <- function(min, max) sonscaling(time=list(min, max, linear.scale))

##' @rdname scaleshortcuts
##' @export
scale_pitch_linear <- function(min, max) sonscaling(pitch=list(min, max, linear.scale))

##' @rdname scaleshortcuts
##' @export
scale_dur_linear <- function(min, max) sonscaling(dur=list(min, max, linear.scale))

##' @rdname scaleshortcuts
##' @export
scale_vol_linear <- function(min, max) sonscaling(vol=list(min, max, linear.scale))

##' @rdname scaleshortcuts
##' @export
scale_pan_linear <- function(min, max) sonscaling(pan=list(min, max, linear.scale))

##' @rdname scaleshortcuts
##' @export
scale_tempo_linear <- function(min, max) sonscaling(tempo=list(min, max, linear.scale))
