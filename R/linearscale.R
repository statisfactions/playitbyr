##' Create a linear parameter scaling with given minimum and maximum
##' 
##' \code{linear_scale()} linearly transforms its input vector \code{x} so that
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
##' @param limits The limits of the data to train, a numeric vector of length
##' 2. All data values outside these limits are returned as
##' \code{NA}. If \code{NULL}, the default, the function takes the
##' minimum and maximum of the data
##' @param soundlimits The limits of the sound parameter.
##' @param by The unit to round the sound parameter to. See examples.
##' @return A numeric vector or matrix of the same type as \code{x},
##' linearly rescaled in the desired way. If \code{x} only has one
##' value, \code{linear_scale} simply returns the midpoint between
##' \code{min} and \code{max}.
##' @seealso \code{\link{sonscaling}}
##' @examples
##' 
##' x <- 1:10
##' linear_scale(x, soundlimits = c(-2, -1.5))
##' 
##' ## If max<min, it's rescaled in reverse:
##' linear_scale(x, soundlimits = c(10, 1))
## TODO add example of choosing limits
## TODO add example of 'by'
##' @export
linear_scale <- function(x, limits = NULL, soundlimits, by = NULL) {
  
  if(soundlimits[1] > soundlimits[2]) {
    ## Allow for reversed polarity
    x <- -x
    soundlimits <- rev(soundlimits)
  }
  if(is.null(limits))
    lims <- c(min(x), max(x))
  else
    lims <- limits
  
  smin <- soundlimits[1]
  smax <- soundlimits[2]
  dmin <- lims[1]
  dmax <- lims[2]

  ## return NA if outside range
  x[x < dmin | x > dmax] <- NA
  
  if(length(unique(x)) == 1 & is.null(limits)) {
    ## allow for all the same, choose midpoint if no limits given
    out <- rep(mean(soundlimits), length(x))
  } else {
    nrange <- abs(smax - smin)
    out <- ((x-dmin)*nrange/(dmax - dmin) + smin)
 }
  if(!is.null(by)) {
    if(!is.numeric(by) | by == 0)
      stop("'by' must be a positive numeric value, or NULL.")
    else
      out <- round(out/by)*by
  }
  out
}

