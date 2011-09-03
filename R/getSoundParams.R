##' Get and check currently possible sound parameters.
##'
##' Gets all supported sound parameters, or checks a character vector
##' of sound parameters for validity. This is used by \code{\link{sonaes}} and
##' \code{\link{sonscaling}} to provide a minimal check that the names
##' of their arguments are valid.
##'
##' @rdname getSoundParams
##' @return \code{getSoundParams} returns a character vector with the
##' name of each parameter. \code{checkSoundParams} returns \code{NULL} if
##' successful.
##' @param shape The shape to return the sound parameters of. The
##' default, \code{"any"}, returns sound parameters of all shapes.
##' @export
##' @keywords internal
getSoundParams <- function(shape = "any") {
  ## Get all param names and condense into a single vector with unique
  ## elements
  paramnames <- lapply(allShapeDefs, function(x) names(x$params)) 

  if(shape %in% "any")
    paramnames <- unique(do.call(c, paramnames))
  else
    paramnames <- paramnames[[shape]]

  return(paramnames)
}

##' @rdname getSoundParams
##' @param paramnames A character vector
##' @export
checkSoundParams <- function(paramnames, shape = "any") {
  badnames <- setdiff(paramnames, getSoundParams())
  if(length(badnames)>0)
    stop("'", paste(badnames, collapse="', '"),
         "' not valid sound parameter(s) for ",
         shape, " shape")
  else return(NULL)
}
