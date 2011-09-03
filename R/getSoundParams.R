##' Get and check currently possible sound parameters.
##'
##' Gets all supported sound parameters, or checks a character vector
##' of sound parameters for validity. This is used by \code{\link{sonaes}} and
##' \code{\link{sonscaling}} to provide a minimal check that the names
##' of their arguments are valid.
##'
##' @rdname getSoundParams
##' @return \code{getSoundParams} returns \code{data.frame} with the
##' name of each parameter and the name of the shape it is a part
##' of. \code{checkSoundParams} returns \code{NULL} if successful.
##' @param shapes The shapes to return the sound parameters of. The
##' default is to get or check all currently available shapes (see
##' \code{link{getShapeNames}}).
##' @export
##' @keywords internal
getSoundParams <- function(shapes = getShapeNames()) {
  ## Get all param names and condense into a single vector with unique
  ## elements
  paramnames <- lapply(allShapeDefs, function(x) names(x$params))
  paramlengths <- lapply(names(allShapeDefs), function(x)
                         rep(x, length(allShapeDefs[[x]]$params)))
  
  params <- cbind(do.call(c, paramnames), do.call(c, paramlengths))
  params <- as.data.frame(params, stringsAsFactors=F)


  names(params) <- c("param", "shape")
  rownames(params) <- NULL

  params <- params[params$shape %in% shapes,]
  return(params)
}

##' @rdname getSoundParams
##' @param paramnames A character vector of sound parameters to check
##' for validity
##' @export
checkSoundParams <- function(paramnames, shapes = getShapeNames()) {
  goodnames <-  getSoundParams(shapes)$param
  badnames <- setdiff(paramnames, goodnames)
  if(length(badnames)>0)
    stop("'", paste(badnames, collapse="', '"),
         "' not valid sound parameter(s) for ",
         "shape(s) '", paste(shapes, collapse="', '"), "'")
  else return(NULL)
}
