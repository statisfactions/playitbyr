##' Setting scales for \code{sonify} objects
##' 
##' \code{sonscaling()} is invoked in the call to \code{sonify} or
##' added onto it in order to specify precisely how the data
##' parameters are mapped onto sonic parameters. Currently, only
##' continuous scales are supported.
##' 
##' Each argument of \code{sonscaling} is in the form of a list, where
##' the first element is the minimum value of the sonic parameter, the
##' second is the maximum, and the third is a function that maps the
##' data column onto the range of the sonic parameter. The only such
##' function included with \pkg{playitbyr} right now is
##' \code{\link{linear.scale}}.
##'
##' \code{\link{scaleShortcuts}} provides a more intuitive interface
##' to defining scales.
##' 
##' @param \dots See Details section.
##' @return A \code{sonscaling} object that 
##' @seealso \code{\link{sonify}} where this is eventually used;
##' \code{\link{sonaes}} for defining the which data columns get
##' mapped onto these sonic parameters; \code{\link{scaleShortcuts}}
##' for easy shortcut functions for common use-cases of scaling; and
##' \code{\link{linear.scale}} for an example of a scaling function.
##' @export
sonscaling <- function(...) {

  out <- list(...)
  
  out <- lapply(out, function(x) {
    if(!is.list(x) | length(x) != 3)
      stop("sonscaling: All arguments must be a list of length 3.",
           call.=F)
    
    names(x) <- c("min", "max", "scaling.function")
    
    if(!is.numeric(x$min) | !is.numeric(x$max)
       | length(x$min) != 1 | length(x$max) != 1)
      stop("sonscaling: The first two elements of each argument must be numbers.",
           call.=F)
    if(!is.function(x$scaling.function))
      stop("sonscaling: The third element of each argument must be a function",
           " such as linear.scale()", call.=F)
    return(x)
  })
  
  if(length(out) > 0
     && is.null(names(out)) | any(names(out) == ""))
    stop("All arguments must be named.")

  checkSoundParams(names(out))

  class(out) <- "sonscaling"
  out
}
