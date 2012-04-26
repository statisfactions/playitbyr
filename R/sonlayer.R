##' Creating individual layers for sonify objects
##'
##' This function creates layers to be added onto a \code{sonify}
##' object. It is most easy to use through the \code{shape_}
##' convenience functions, such as \code{\link{shape_scatter}}.
##' 
##' @section Conflicting defaults on \code{sonlayer} scalings: Each shape has
##' its own default scalings (see \code{link{.getShapeDef}} to view
##' these defaults. It's quite possible that some default scalings for
##' a parameter differ between scalings; if this is the case, the
##' default scaling for the earlier \code{sonlayer} type, as it appears in the
##' \code{$sonlayers} slot of a \code{sonify} object, takes precedence.
##'
##' Default scalings are, of course, ignored when they are explicitly
##' declared; if you don't like the behavior of a default scaling you
##' can always define your own (see \code{\link{sonscaling}}.
##' 
##' @rdname sonlayer
##' @export 
##' @param shape A character string representing the overall style of
##' the audio plot (analogous to \code{geom}s from the \pkg{ggplot2}
##' package).
##' @param data The \code{data.frame} to be sonified for this
##' layer. If blank, the data from the parent \code{sonify} object is
##' used.
##' @param mapping A \code{\link{sonaes}} object.
##' @param \dots Additional options and settings specific to the shape
##' @keywords internal
##' @seealso This functionality is most easily accessed through its
##' shortcut functions, the \code{shape_}\kbd{shapename} functions, currently: \code{\link{shape_scatter}}
# currently excluded \code{\link{shape_csound}}, \code{\link{shape_dotplot}}

sonlayer <- function(shape="scatter", data=NULL, mapping=NULL, ...) {
  if(!(shape %in% .getShapeNames()))
     stop("'", deparse(shape),"' is not a valid shape name. See .getShapeNames.")

  ## these 'stat' parameters are placeholders right now
  ## @param stat The statistic to be calculated for the layer (currently ignored)
  ## @param stat_params Additional parameters specific to the stat (currently ignored)
  
  stat <- NULL
  stat_params <- NULL

  ## Check shape params given
  shape_params <- list(...)
  nonsound_params <- names(formals(paste("shape_", shape, sep = ""))) # grabs the other parameters specified in the 'shape_' shortcut function for checking
  sound_params <- setdiff(shape_params, nonsound_params)
  .checkSoundParams(names(sound_params), shape)

  l <- list(list(shape, shape_params), list(stat, stat_params), data, mapping)
  
  names(l) <- c("shape", "stat", "data", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- "sonlayer"

  .checkData(l)    
  l
}
