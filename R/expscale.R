##' Create a exponential parameter scaling with given minimum and maximum
##' 
##' \code{exp_scale()} exponentially transforms its input vector \code{x} so that
##' its minimum value is \code{min} and its maximum value is \code{max}. This
##' is for use as a scaling function in \code{\link{sonscaling}}. It is
##' unlikely to be directly called by the user.
##' 
##' This simple function does the important work of actually rescaling the
##' given data of a \code{sonify} object into the range of the given sonic
##' parameter, which is why it's included in \pkg{playitbyr}. But it can be
##' used on any vector or matrix. 
##'
##' @rdname expscale
##' @inheritParams linear_scale
##' @return A numeric vector or matrix of the same type as \code{x}, exponentially
##' rescaled in the desired way.
##' @keywords internal
##' @seealso \code{\link{linear_scale}} for more details on how scaling functions work
##' @examples
##' 
##' x <- 1:10
##' exp_scale(x, soundlimits = c(-2, -1.5))
##' 
##' ## If max<min, it's rescaled in reverse:
##' exp_scale(x, soundlimits = c(10, 1))
##'
##' @export
exp_scale <- function(x, limits = NULL, soundlimits, by = NULL) {
  ## Exponentially rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum
  dmin <- limits[1]
  dmax <- limits[2]

  ## return NA if outside range
  x[x < dmin | x > dmax] <- NA
  
  if(any(na.exclude(c(x, limits) <= 0)))
    stop("'x' and 'limits' must be greater than zero for exponential scales (try setting 'limits' to a positive number to exclude non-positive values)")
  lx <- log(x)
  if(!is.null(limits))
     limits <- log(limits)
  return(linear_scale(lx, limits, soundlimits, by))
}
