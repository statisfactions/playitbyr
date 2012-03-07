##' Continuous vol scales
##'
##' These shortcut functions (with the notation
##' \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
##' function]}) are intended to be added onto a \code{sonify} object
##' to specify how \kbd{[data parameter]} from the data (a column of
##' the data) is to be mapped to sonic parameters (here,
##' \code{"vol"}).
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
##' @rdname scale_vol_continuous
##' @export
scale_vol_exp <- function(min, max, dmin = NULL, dmax = NULL, ...) exp_fixed_scale("vol", min, max, dmin, dmax)

##' @rdname scale_vol_continuous
##' @export
scale_vol_continuous <- function(min, max, dmin = NULL, dmax = NULL, ...) linear_fixed_scale("vol", min, max, dmin, dmax)
