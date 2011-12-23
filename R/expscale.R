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
##' @param x A numeric vector or matrix
##' @param min The desired minimum value, a \code{numeric} of length
##' 1. Must be greater than zero.
##' @param max The desired maximum value, a \code{numeric} of length
##' 1. Must be greater than zero.
##' @return A numeric vector or matrix of the same type as \code{x}, exponentially
##' rescaled in the desired way.
##' @seealso \code{\link{sonscaling}}, \code{\link{scaleShortcuts}}
##' @examples
##' 
##' x <- 1:10
##' exp_scale(x, min=-2, max=-1.5)
##' 
##' ## If max<min, it's rescaled in reverse:
##' exp_scale(x, min=10, max=1)
##'
##' @export
exp_scale <- function(x, min, max) {
  ## Exponentially rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum
  if(any(x <= 0))
    stop("All of vector 'x' must be greater than zero.")
  if(min>max) {
    ## Allow for reversed polarity
    x <- -x
    oldmin <- min
    oldmax <- max
    min <- oldmax
    max <- oldmin
  }
  lx <- log(x)
  return(linear_scale(lx, min, max))
}
