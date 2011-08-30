##' Create a linear parameter scaling with given minimum and maximum
##' 
##' \code{linear.scale()} linearly transforms its input vector \code{x} so that
##' its minimum value is \code{min} and its maximum value is \code{max}. This
##' is for use as a scaling function in \code{\link{sonscaling}}. It is
##' unlikely to be directly called by the user.
##' 
##' This simple function does the important work of actually rescaling the
##' given data of a \code{sonify} object into the range of the given sonic
##' parameter, which is why it's included in \pkg{playitbyr}. But it can be
##' used on any vector or matrix.
##'
##' @rdname linearscale
##' @param x A numeric vector or matrix
##' @param min The desired minimum value, a \code{numeric} of length 1
##' @param max The desired maximum value, a \code{numeric} of length 1
##' @return A numeric vector or matrix of the same type as \code{x}, linearly
##' rescaled in the desired way.
##' @seealso \code{\link{sonscaling}}, \code{\link{scaleShortcuts}}
##' @examples
##' 
##' x <- 1:10
##' linear.scale(x, min=-2, max=-1.5)
##' 
##' ## If max<min, it's rescaled in reverse:
##' linear.scale(x, min=10, max=1)
##'
##' @export
linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum

  if(min>max) {
    ## Allow for reversed polarity
    x <- -x
    oldmin <- min
    oldmax <- max
    min <- oldmax
    max <- oldmin
  }
  
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}
