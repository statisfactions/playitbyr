##' Add additional layers or options onto sonify objects
##'
##' \code{\link{sonify}} objects can be incrementally added onto to form a
##' complete sonification, much like \code{ggplot} objects.
##'
##' Adding mappings, layers, facets, and other parameters onto a
##' \code{sonify} object is the easiest method of creating a
##' sonification. The following kinds of objects can be added:
##' 
##' \describe{
##' 
##' \item{\code{\link{sonaes}}}{Mappings of data columns to sound parameters}
##' 
##' \item{\code{\link{sonlayer}}}{Layers, such as \code{\link{shape_scatter}}}
##' 
##' \item{\code{\link{sonscaling}}}{Scalings, such as \code{\link{scale_time_continuous}}}
##' 
##' \item{\code{\link{sonfacet}}}{Instructions for faceting the sonification}
##' 
##' \item{\code{\link{sonopts}}}{Additional options}
##' 
##' }
##'
##' @rdname add-sonify
##' @param x A \code{sonify} object
##' @param y An object to be added into the sonify object (see Details).
##' @return A sonify object with
##' the relevant \code{y} value added into the object.
##' @seealso \code{\link{sonify}} for the creation of these objects
##' @method + sonify
##' @export
`+.sonify` <- function(x, y) {
  if("sonlayer" %in% class(y)) {
    ## adds sonlayer
    if(is.null(x$sonlayers)) {
      x$sonlayers[[1]] <- y
    } else {x$sonlayers <- c(x$sonlayers, list(y))}
  } else if("sonscaling" %in% class(y)) {
    ## adds to or overrides scale
    for(i in names(y)) {
      x$scales[[i]] <- y[[i]]
    }
  } else if("sonaes" %in% class(y)) {
    for(i in names(y)) {
      x$mapping[[i]] <- y[[i]]
    }
    if(is.null(y$time) && !is.null(x$mapping$tempo))
      x$mapping["time"] <- list(NULL)
    if(is.null(y$tempo) & !is.null(x$mapping$time))
      x$mapping["tempo"] <- list(NULL)
  } else if("sonopts" %in% class(y)) {
    ## adds to or overrides sonopts
    for(i in names(y)) {
      x$opts[[i]] <- y[[i]]
    }
  } else if("sonfacet" %in% class(y)) {
    x$sonfacet <- y
  } else stop("'+' operator not supported for this operation.")
  x
}         




