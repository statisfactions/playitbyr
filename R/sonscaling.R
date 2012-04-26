##' Setting scales for \code{sonify} objects
##' 
##' \code{sonscaling()} is invoked in the call to \code{sonify} or
##' added onto it in order to specify precisely how the data
##' parameters are mapped onto sonic parameters. It is most easy to
##' use via the \code{scale_} convenience functions, such as
##' \code{\link{scale_time_continuous}}.
##' 
##' Each argument of \code{sonscaling} is in the form of a list, where
##' the first element is the minimum value of the sonic parameter, the
##' second is the maximum, and the third is a function that maps the
##' data column onto the range of the sonic parameter. The only such
##' function included with \pkg{playitbyr} right now is
##' \code{\link{linear_scale}}.
##'
##' 
##' @param \dots See Details section.
##' @return A \code{sonscaling} object 
##' @seealso \code{\link{sonify}} where this is eventually used;
##' \code{\link{sonaes}} for defining the which data columns get
##' mapped onto these sonic parameters; 
##' \code{\link{linear_scale}} for an example of a scaling function.
##' @keywords internal
##' @export
sonscaling <- function(...) {

  out <- list(...)
  
  out <- lapply(out, sonscale)
  
  if(length(out) > 0
     && is.null(names(out)) | any(names(out) == ""))
    stop("All arguments must be named.")

  .checkSoundParams(names(out))

  class(out) <- "sonscaling"
  out
}

sonscale <- function(x) {
  if(!is.list(x))
    stop("All arguments to sonscaling() must be a list")
  else if(length(x) == 3) {
    names(x) <-  c("limits", "soundlimits", "scaling.function")
  } else stop("Each argument must be a list of length 2 or 3.")

  if((!is.numeric(x$limits) | length(x$limits) != 2) & (!is.null(x$limits)))
    stop("All arguments' 'limits' slot must be numeric of length 2, or NULL")

  if(!is.numeric(x$soundlimits) | length(x$soundlimits) != 2)
    stop("All arguments' 'soundlimits' slot must be of length 2")

  if(!is.function(x$scaling.function))
    stop("sonscaling: The third element of each argument must be a function",
         " such as linear_scale()", call.=F)
  
  return(x)
}
     

