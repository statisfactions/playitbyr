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
##' @return A numeric vector or matrix of the same type as \code{x},
##' linearly rescaled in the desired way. If \code{x} only has one
##' value, \code{linear_scale} simply returns the midpoint between
##' \code{min} and \code{max}.
##' @seealso \code{\link{sonscaling}}
##' @examples
##' 
##' x <- 1:10
##' linear_scale(x, min=-2, max=-1.5)
##' 
##' ## If max<min, it's rescaled in reverse:
##' linear_scale(x, min=10, max=1)
## TODO add example of choosing midpoint and document here
##' @export
linear_scale <- function(x, limits = NULL, soundlimits) {
  
  if(soundlimits[1] > soundlimits[2]) {
    ## Allow for reversed polarity
    x <- -x
    soundlimits <- rev(soundlimits)
  }
  if(is.null(limits))
    limits <- c(min(x), max(x))
  
  smin <- soundlimits[1]
  smax <- soundlimits[2]
  dmin <- limits[1]
  dmax <- limits[2]

  ## return NA if outside range
  x[x < dmin | x > dmax] <- NA
  
  if(length(unique(x)) == 1) {
    ## allow for all the same, choose midpoint
    out <- rep(mean(soundlimits), length(x))
  } else {
    nrange <- abs(smax - smin)
    out <- ((x-dmin)*nrange/(dmax - dmin) + smin)
  }
  out
}


fixie <- function(column, min, max) {
  dmin <- 1
  dmax <- 3
  exp_scale(c(dmin, dmax, column), min, max)[-(1:2)]
}

exp_fixed_scale <- function(param, min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, exp_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    exfixie <- fixie
    body(exfixie)[[2]][[3]] <- dmin
    body(exfixie)[[3]][[3]] <- dmax
    out <- sonscaling("start" = list(min, max, exfixie))
  }
  names(out) <- param
  out
}


## returns vector rounded to the nearest twelfth for use with
## chromatic scales This whole section is a horrible cheap hack and I
## feel very sorry for anyone trying to figure this out and apologize
## humbly. Really, I should be able to pass arbitrary parameters to
## scaling functions is the real lesson here.
round_chromatic <- function(x) round(x*12)/12

chromfixie <- function(column, min, max) {
  dmin <- 1
  dmax <- 3
  exp_scale(c(dmin, dmax, column), min, max)[-(1:2)] -> out
  round_chromatic(out)
}

linear_fixed_scale_chromatic <- function(min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, linear_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    linfixie <- chromfixie
    body(linfixie)[[2]][[3]] <- dmin
    body(linfixie)[[3]][[3]] <- dmax
    body(linfixie)[[4]][[3]][[2]][[1]] <- substitute(linear_scale)
    out <- sonscaling("start" = list(min, max, linfixie))
  }
  names(out) <- "pitch"
  out
}

exp_fixed_scale_chromatic<- function(min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, exp_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    exfixie <- chromfixie
    body(exfixie)[[2]][[3]] <- dmin
    body(exfixie)[[3]][[3]] <- dmax
    out <- sonscaling("start" = list(min, max, exfixie))
  }
  names(out) <- "pitch"
  out
}

