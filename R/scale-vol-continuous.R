##' Continuous note volume scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here, \code{"vol"},
##' the proportion of the maximum volume of the note, as a number
##' between 0 and 1).
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##'
##' @return A \code{sonscaling} object, to be used in \code{sonify} or
##' added onto a \code{sonify} object.
##' 
##' @inheritParams scale_time_continuous
##' @param \dots Other parameters (currently ignored)
##' @seealso \code{\link{shape_scatter}}, which has more information
##' about this parameter
##' @rdname scale_vol_continuous
##' @export
scale_vol_exp <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(vol = list(limits, soundlimits, function(x, limits, soundlimits) exp_scale(x, limits = limits, soundlimits, by = by)))

##' @rdname scale_vol_continuous
##' @export
scale_vol_continuous <- function(soundlimits, limits = NULL, by = NULL, ...) sonscaling(vol = list(limits, soundlimits, function(x, limits, soundlimits) linear_scale(x, limits = limits, soundlimits, by = by)))


##' 
