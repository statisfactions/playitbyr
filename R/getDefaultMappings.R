##' Get the default mappings and scalings for a given sonlayer shape
##'
##' Given a \code{shape} name, these functions returns the default
##' mappings or scalings for that shape.
##'
##' @rdname getDefaultMappings
##' @param shape A character containing the name of the shape to be
##' looked up
##' @return The default mappings or default scalings for a given
##' shape.
##' @export
##' @keywords internal
getDefaultMappings <- function(shape) {
  shapedef <- getShapeDef(shape)
  defaults <- lapply(shapedef$params, function(x) x$defaultSetting)
  return(do.call(sonaes, defaults))
}

##' @rdname getDefaultMappings
##' @export
getDefaultScalings <- function(shape) {
  shapedef <- getShapeDef(shape)
  defaults <- lapply(shapedef$params, function(x) x$defaultScaling)
  return(do.call(sonscaling, defaults))
}
                     

