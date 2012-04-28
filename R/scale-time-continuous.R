##' Continuous scales for note starting times
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here
##' \code{"time"}, the starting time of the note in seconds).
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##' @return A \code{sonscaling} object, to be used in \code{sonify} or added
##' onto a \code{sonify} object.
##' @param limits The limits of the data to train, a numeric vector of length
##' 2. All data values outside these limits are returned as
##' \code{NA}. If \code{NULL}, the default, the function takes the
##' minimum and maximum of the data
##' @param soundlimits The limits of the sound parameter.
##' @param by The unit to round the sound parameter to.
##' @param \dots Other parameters (currently ignored)
##'
##' @seealso \code{\link{shape_scatter}}, which has more information
##' about this parameter
##' @rdname scale_time_continuous
##' @export
scale_time_continuous <- function(soundlimits, limits = NULL, by = NULL, ...)
  sonscaling(time = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_time_continuous
##' @export
scale_time_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(time = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))



##' 
