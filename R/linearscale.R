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
##' @keywords internal
##' @note If \code{by} is specified, unexpected results may happen when \code{soundlimits[2] != (soundlimits[1] + n * by)} for some \code{n}; in these cases the data will be trained to the largest value of \code{(soundlimits[1] + n * by)} less than \code{soundlimits[2]}.
##' @examples
##' 
##' x <- 1:10
##' linear_scale(x, soundlimits = c(-2, -1.5))
##' 
##' ## If max<min, it's rescaled in reverse:
##' linear_scale(x, soundlimits = c(10, 1))
##'
##' ## If 'limits' is specified, these are taken as the maximum and
##' ## minimum of the data to be rescaled to
##' linear_scale(x, limits = c(-10, 10), soundlimits = c(10, 0))
##'
##' ## any values outside of 'limits' are therefore NA in result
##' linear_scale(x, limits = c(1, 2), soundlimits = c(1, 10))
##'
##' ## 'by' rounds values appropriately
##' linear_scale(x, soundlimits = c(1, 2), by = .25)
##' @export
linear_scale <- function(x, limits = NULL, soundlimits, by = NULL) {
  
  if(soundlimits[1] > soundlimits[2]) {
    ## Allow for reversed polarity
    x <- -x
    if(!is.null(limits))
      limits <- -rev(limits)
    soundlimits <- rev(soundlimits)
  }
  if(is.null(limits))
    lims <- c(min(x, na.rm = T), max(x, na.rm = T))
  else
    lims <- limits
  
  smin <- soundlimits[1]
  smax <- soundlimits[2]
  dmin <- lims[1]
  dmax <- lims[2]

  ## NA if outside range
  x[x < dmin | x > dmax] <- NA
  
  if(length(unique(na.omit(x))) == 1) {
    ## allow for all the same, choose midpoint if no limits given
    out <- rep(mean(soundlimits), length(x))
    out[is.na(x)] <- NA
  } else {
    nrange <- abs(smax - smin)
    out <- ((x-dmin)*nrange/(dmax - dmin) + smin)
 }
  if(!is.null(by)) {
    if(!is.numeric(by) | by <= 0)
      stop("'by' must be a positive numeric value, or NULL.")
    else
      out <- round((out -smin)/by)*by + smin
  }


  out
}

