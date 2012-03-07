##' Continuous modulation multiplier scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here, \code{"mod"},
##' the modulation frequency as a multiple of the carrier frequency).
##'
##' \code{_continuous} scales are a linear scaling, whereas
##' \code{_exp} is an exponential scale.
##'
##' @return A \code{sonscaling} object, to be used in \code{sonify} or
##' added onto a \code{sonify} object.
##' 
##' @seealso \code{\link{sonscaling}} for the full syntax and range of
##' possibilities for specifying scaling of \code{sonify} objects;
##' \code{\link{linear_scale}} \code{\link{exp_scale}} for the
##' pre-defined scaling functions which this incorporates;
##' \code{\link{+.sonify}} for the addition onto \code{sonify}
##' objects.
##'
##' @inheritParams scale_time_continuous
##' @rdname scale_mod_continuous
##' @export
scale_mod_continuous <- function(min, max, dmin = NULL, dmax = NULL, ...) linear_fixed_scale("mod", min, max, dmin, dmax)

##' @rdname scale_mod_continuous
##' @export
scale_mod_exp <- function(min, max, dmin = NULL, dmax = NULL, ...) exp_fixed_scale("mod", min, max, dmin, dmax)
