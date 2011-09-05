##' Creating individual layers for sonify objects
##'
##' These functions are a start on the layer functionality for \code{sonify}
##' objects, analogous to \pkg{ggplot2} layers. This creates a list
##' object that can be included in a \code{sonify} object.
##'
##' @section Conflicting defaults on \code{sonlayer} scalings: Each shape has
##' its own default scalings (see \code{link{getShapeDef}} to view
##' these defaults. It's quite possible that some default scalings for
##' a parameter differ between scalings; if this is the case, the
##' default scaling for the earlier \code{sonlayer} type, as it appears in the
##' \code{$sonlayers} slot of a \code{sonify} object, takes precedence.
##'
##' Default scalings are, of course, ignored when they are explicitly
##' declared; if you don't like the behavior of a default scaling you
##' can always define your own (see \code{\link{sonscaling}} and
##' \code{\link{scaleShortcuts}}.
##' 
##' @aliases sonlayer shape_notes
##' 
##' @rdname sonlayer
##' @export 
##' @param shape A character string representing the overall style of
##' the audio plot (analogous to \code{geom}s from the \pkg{ggplot2}
##' package).
##' @param shape_params Additional parameters specific to the shape
##' @param stat The statistic to be calculated for the layer
##' @param stat_params Additional parameters specific to the stat
##' @param data The \code{data.frame} to be sonified for this
##' layer. If blank, the data from the parent \code{sonify} object is
##' used.
##' @param mapping A \code{\link{sonaes}} object.

sonlayer <- function(shape="notes", shape_params=NULL, stat=NULL,
                     stat_params=NULL, data=NULL, mapping=NULL) {
  if(!(shape %in% getShapeNames()))
     stop("'", deparse(shape),"' is not a valid shape name. See getShapeNames.")

  dataname <- deparse(substitute(data)) # Used by summary.sonify()
  l <- list(list(shape, shape_params), list(stat, stat_params), data, dataname, mapping)
  
  names(l) <- c("shape", "stat", "data", "dataname", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- "sonlayer"

  .checkData(l)    
  l
}
##' @rdname sonlayer
##' @export
##' @param \dots Layer parameters to be passed to \code{sonlayer}
shape_notes <- function(...) sonlayer("notes",...)
## Convenience function for the only supported layer type, notes.
