##' Render a sonification score to sound
##'
##' \code{render} is a generic which takes a \code{score} object
##' created by \code{\link{.getScore}} and renders it to sound.
##'
##' The class of the \code{score} object \code{x} determinines object,
##' which determines the render method called. Currently only
##' \code{"audio"} is available for a rendering method, but plans for
##' csound  are in the works.
##' 
##' @param x A \code{score} object created by \code{\link{.getScore}}
##' @param audioSample Logical indicating whether to return an
##' \code{audioSample} object containing the from the object.
##' @param \dots Arguments to pass to the specific render method.
##' @return If \code{audioSample=TRUE}, an audioSample object
##' containing the sound of the rendering; otherwise NULL
##'
##' @keywords internal
render <- function(x, audioSample=FALSE, ...) UseMethod("render")

