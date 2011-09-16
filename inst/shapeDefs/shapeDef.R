##' Construction functions for a new \code{shapedef}
##'
##' These functions facilitate the creation of a new kind of
##' \code{shape} by providing functions to construct a new
##' \code{shapeDef} object which contains everything \pkg{playitbyr}
##' needs to keep track of mappings, scalings, and eventually
##' statistical transformations for the shape.
##'
##' To create a new shape, first create each parameter using
##' \code{shapeParam}. Then, use \code{shapeDef} to bind everything
##' together in an object. Then, save the object (using \code{save()})
##' as a \code{rda} file in the \code{inst/shapeDefs} directory of the
##' package. Now, when you install the package the shapeDef will be
##' automatically loaded (see aaa.R for how this is
##' done). Unfortunately the wherewithal to implement stats is not
##' quite ready.
##'
##' If you are creating a shapeDef that shares many properties of an
##' existing one, you can simply copy the existing one and change it,
##' if you find that easier. Also in this directory is the code used
##' to generate the \sQuote{notes} \code{shapeDef}.
##'
##' @rdname shapeDef
##' @param shape A string containing name of the shape (e.g. \sQuote{\code{notes}})
##' @param description A string containing a description of the shape
##' @param renderings A character vector containing the rendering
##' methods (see \code{link{rendering}}) that support this shape.
##' @param options A named list of the options and their default values
##' @param params A named list of the sound parameters created by shapeParam
##' @return \code{shapeDef} returns a new shapeDef object that for use
##' by \code{\link{checkSonify}} and \code{\link{getShapeDef}};
##' \code{shapeParam} returns a named list for use in the
##' \code{params} argument of \code{shapeDef}
##' 
shapeDef <- function(description, renderings, options, params) {

  x <- list(description, renderings, options, params)
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
##' @param defaultStatName The default statistic. Currently ignored,
##' since the infrastructure isn't yet up and running.
##' @param description A string containing the description of the parameter
shapeParam <- function(param, min, max, defaultSetting, defaultScaling, defaultStatName, description) {
  if(!is.null(defaultScaling))
    names(defaultScaling) <- c("min", "max", "scaling.function")
  
  x <- list(list(min, max, defaultSetting, defaultScaling, defaultStatName, description))
  names(x) <- param
  names(x[[param]]) <- names(formals())[-1]
  return(x)
}

