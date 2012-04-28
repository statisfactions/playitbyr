##' Check if a \code{sonify} object can be rendered
##' 
##' This function is called by \code{\link{print.sonify}} before rendering to
##' check if the object can be rendered; it can also be called directly by the
##' user for diagnostic purposes.
##'
##' @rdname checkSonify
##' @param x A \code{sonify} object.
##' @keywords internal
##' @seealso \code{\link{sonify}}, \code{\link{sonaes}},
##' \code{\link{sonscaling}} all give more details on how to correctly form a
##' \code{sonify} object.
##' @export
.checkSonify <- function(x) {
  xname <- deparse(substitute(x))
  
  ## Check to make sure there are layers to render
  if(is.null(x$sonlayers))
    stop("Cannot render sound without any sonlayers.")
  
  if(!("sonify" %in% class(x)))
    stop("'",xname,"' is not a 'sonify' object.")

  ## is the facet actually in the sonify object?
  if(!is.null(x$sonfacet) && !(x$sonfacet$facet %in% names(x$data)))
    warning("The specified facet variable '", x$sonfacet$facet, "' is not in the default data for sonification (i.e. that specified in the call to 'sonify()'; not necessarily a problem but may cause unexpected results and errors.")
  ## Do any layers of x contain data?
  layers.null <- all(sapply(x$sonlayers, function(y) is.null(y["data"])))
  if(is.null(x$data) & layers.null)
    stop("No data.frame provided for sonification. See ?sonify.")
  
}
