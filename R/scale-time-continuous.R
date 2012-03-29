##' Continuous time scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters.
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##' @return A \code{sonscaling} object, to be used in \code{sonify} or added
##' onto a \code{sonify} object.
##' @seealso \code{\link{sonscaling}} for the full syntax and range of
##' possibilities for specifying scaling of \code{sonify} objects;
##' \code{\link{linear_scale}} \code{\link{exp_scale}} for the
##' pre-defined scaling functions which this incorporates;
##' \code{\link{+.sonify}} for the addition onto \code{sonify}
##' objects.
##'
##' @param limits The limits of the data to train, a numeric vector of length
##' 2. All data values outside these limits are returned as
##' \code{NA}. If \code{NULL}, the default, the function takes the
##' minimum and maximum of the data
##' @param soundlimits The limits of the sound parameter.
##' @param by The unit to round the sound parameter to. See examples.
##' @param \dots Other parameters (currently ignored)
##'
##' @rdname scale_time_continuous
##' @export
scale_time_continuous <- function(soundlimits, limits = NULL, by = NULL, ...)
  sonscaling(time = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_time_continuous
##' @export
scale_time_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(time = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))

