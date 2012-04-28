##' Continuous note duration scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here,
##' \code{"dur"}, the length of the resulting note).
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##'
##' @return A \code{sonscaling} object, to be used in \code{sonify} or
##' added onto a \code{sonify} object.
##' 
##' @inheritParams scale_time_continuous
##' @seealso \code{\link{shape_scatter}}, which has more information
##' about this parameter
##' @param \dots Other parameters (currently ignored)
##' @rdname scale_dur_continuous
##' @export
scale_dur_continuous <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(dur = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_dur_continuous
##' @export
scale_dur_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(dur = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))


##' 
