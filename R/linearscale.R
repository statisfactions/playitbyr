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
##' @param min The desired minimum value, a \code{numeric} of length 1
##' @param max The desired maximum value, a \code{numeric} of length 1
##' @return A numeric vector or matrix of the same type as \code{x}, linearly
##' rescaled in the desired way.
##' @seealso \code{\link{sonscaling}}, \code{\link{scaleShortcuts}}
##' @examples
##' 
##' x <- 1:10
##' linear_scale(x, min=-2, max=-1.5)
##' 
##' ## If max<min, it's rescaled in reverse:
##' linear_scale(x, min=10, max=1)
##'
##' @export
linear_scale <- function(x, min, max) {
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

linear_fixed_scale <- function(param, min, max, dmin, dmax) {
  numdmin <- as.numeric(!is.null(dmin)) + as.numeric(!is.null(dmax))
  if(numdmin == 0)
    out <- sonscaling(start = list(min, max, linear_scale))
  else if(numdmin == 1)
    stop("Only one of dmin, dmax was given. Both or neither can be supplied.")
  else {
    linfixie <- fixie
    body(linfixie)[[2]][[3]] <- dmin
    body(linfixie)[[3]][[3]] <- dmax
    body(linfixie)[[4]][[2]][[1]] <- substitute(linear_scale)
    out <- sonscaling("start" = list(min, max, linfixie))
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

