##' Get the \code{shapeDef} object for a shape
##'
##' Each shape has a \code{shapeDef} object associated with it,
##' which is  a list specifying many aspects of the shape and its
##' default; this function takes a sonlayer object or a character
##' string and gets the appropriate shapeDef.
##'
##' @param x A \code{\link{sonlayer}} object or character string
##' @return A \code{shapeDef} object
##' @export
##' @keywords internal
getShapeDef <- function(x) {
  if(is.character(x))
    shape <- x
  else shape <- class(x)
  
  return(allShapeDefs[[shape]])
}


