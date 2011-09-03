##' Show the names and descriptions of all available shapes
##'
##' A simple function to show all the currently available shapes,
##' descriptions, and available sonic parameters for mapping data onto.
##'
##' @return A \code{data.frame} with each row representing one of the
##' shapes available for sonification, a brief description of the
##' shape, and the parameters available for sonification. 
##' @export
shapeDescriptions <- function() {
  descriptions <- sapply(allShapes, function(x) x$description)
  params <- sapply(allShapes, function(x) paste(names(x$params), collapse=", "))
  return(cbind(descriptions, params))
}
  
