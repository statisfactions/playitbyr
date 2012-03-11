##' Get the default settings and scalings for a given sonlayer shape
##'
##' Given a \code{shape} name, these functions returns the default
##' settings or scalings for that shape.
##'
##' @rdname getDefaultSettings
##' @param shape A character containing the name of the shape to be
##' looked up
##' @return The default settings or default scalings for a given
##' shape.
##' @export
##' @keywords internal
.getDefaultSettings <- function(shape) {
  shapedef <- .getShapeDef(shape)
  defaults <- lapply(shapedef$params, function(x) x$defaultSetting)
  return(do.call(sonaes, defaults))
}
