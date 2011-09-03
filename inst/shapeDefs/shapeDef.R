##' Construction functions for a new \code{shapedef}
##'
##' These functions facilitate the creation of a new kind of
##' \code{shape} by providing functions to construct a new
##' \code{shapeDef} object which contains everything \pkg{playitbyr}
##' needs to keep track of mappings, scalings, and error-checking for
##' the shape.
##'
##' @rdname shapeDef
##' @param shape A string containing name of the shape (e.g. \sQuote{\code{notes}})
##' @param description A string containing a description of the shape
##' @param renderings A character vector containing the rendering
##' methods (see \code{link{rendering}}) that support this shape.
##' @param params A named list of the sound parameters created by newParam
##' @return \code{shapeDef} returns a new shapeDef object that for use
##' by \code{\link{checkSonify}} and \code{\link{getShapeDef}};
##' \code{shapeParam} returns a named list for use in the
##' \code{params} argument of \code{shapeDef}
##' 
shapeDef <- function(description, renderings, params) {

  x <- list(description, renderings, params)
  names(x) <- names(formals())
  class(x) <- "shapeDef"

  return(x)
}

##' @rdname shapeDef
##' @param param the name of the individual parameter
##' @param min The minimum value that this parameter can take
##' @param max The maximum value that this parameter can take
##' @param defaultSetting The default numeric setting of this
##' variable, used if no setting or mapping is specified.
##' @param defaultScaling The default scaling (as created by
##' \code{\link{sonscaling}} for this parameter
##' @param description A string containing the description of the parameter
shapeParam <- function(param, min, max, defaultSetting, defaultScaling, description) {
  names(defaultScaling) <- c("min", "max", "scaling.function")
  x <- list(list(min, max, defaultSetting, defaultScaling, description))
  names(x) <- param
  names(x[[param]]) <- names(formals())[-1]
  return(x)
}
  
