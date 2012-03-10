##' Internal functions to generate output \code{data.frame} from a
##' \code{sonify} object.
##' 
##' These functions, together, are used to generate a standardized
##' \code{data.frame}, rather like a Csound score, that is then used
##' by whatever method renders the sounds (currently only rendering in
##' R as sine waves is supported).
##'
##' @note These functions (particularly .getSonlayerScore) assume that
##' every shape has a single output row for each input data row after
##' applying statistical tranformations
##' 
##' @rdname internaldf
##' @name internaldf
##' @aliases .getScore .getSonlayerMappings .getSonlayerData
##' .getSonlayerScore
##' @param x A \code{sonify} object
##' @param sonlayernum The layer number of the \code{sonify}
##' object. This is a placeholder for future implementation of a
##' layering functionality (modeled after \code{ggplot2} objects).
##' @return The end product of all this is a \code{data.frame} object
##' that is used as an input to the rendering process
##' @keywords internal
##' 
.getScore <- function(x) {
  ## Returns a data.frame score that the render method
  ## uses to actually create the sound for the sonification

  ## Check to make sure there are layers to render
  if(is.null(x$sonlayers))
    stop("Cannot render sound without any sonlayers.")

  ## Create a score for each sonlayer and put together in list
  score <- lapply(1:length(x$sonlayers),
                  function(layernum) .getSonlayerScore(x, layernum))

  ## Get the total length in seconds of the sonification (i.e. the
  ## longest of the layers) and pass as an attribute.
  length <- max(sapply(score, function(y) attributes(y)$length))
  attr(score, "length") <- length
  attr(score, "render_options") <- x$render_options

  ## The class of score is used to determine which rendering method is
  ## called. 
  class(score) <- c(x$rendering, "score")
  
  return(score)
}

##' @rdname internaldf
.getSonlayerScore <- function(x, sonlayernum) {
  data <- .getSonlayerData(x, sonlayernum, transform = TRUE)
  if(!is.null(x$sonfacet)) {
    if(x$sonfacet$facet %in% names(data)) {
    datal <- split(data, data[[x$sonfacet$facet]], drop = TRUE)
    nl <- length(datal)
    scorel <- lapply(datal, function(d)
                     .getSonlayerScoreFacet(x, sonlayernum, d))
    facetend <- sapply(scorel, function(s) attr(s, "length"))
    facetend <- facetend + (0:(nl-1))*x$sonfacet$pause
    scorelshift <- lapply(1:nl, function(fn) {
      outsc <- scorel[[fn]]
      if(fn > 1) 
        outsc$start <- outsc$start + facetend[fn]
      return(outsc)
    })
    score <- do.call(rbind, scorelshift)
    attr(score, "length") <- facetend[nl]
  }} else {
    score <- .getSonlayerScoreFacet(x, sonlayernum, data)
  }

  ## Set shape to pass to shape and rendering methods
  class(score) <- c(.getSonlayerShape(x, sonlayernum),  "data.frame")

  ## Add shape options, if any, to pass to rendering methods
  attr(score, "shape_params") <- .getSonlayerShapeOptions(x, sonlayernum)
  ## TODO subtract out sound params from this function
  score  
}

##' @rdname internaldf
.getSonlayerScoreFacet <- function(x, sonlayernum, data) {
  ## Returns an output data.frame with all the information needed to
  ## render the sonlayernum-th sonlayer of x.  The output is in a
  ## format rather similar to a Csound score.

  ## Get mappings and stat-transformed data
  map <- .getSonlayerMappings(x, sonlayernum, remove.null = TRUE) 
  set <- .getSonlayerSettings(x, sonlayernum, remove.null = TRUE)
  shape <- .getSonlayerShape(x, sonlayernum)

  n <- nrow(data) # length of output score

  if(n == 0)
    ## if no data here, skip this facet
    return(NULL)

  ## Create output score:
  ## If map gives data.frame column, rescale that column.
  ## If map gives anything else, create new variable and then rescale column.
  out <- lapply(names(map), function(param) {
    if(length(map[[param]]) == 1 && map[[param]] %in% names(data))
      column <- data[[map[[param]]]]
    else
      column <- eval(map[[param]])
    if(!is.numeric(column))
      column <- as.numeric(factor(column))
    return(.rescaleDataByParam(x, param, column))
  })

  ## add on settings for params that do not yet have columns
  names(out) <- names(map)
  missingmaps <- setdiff(names(set), names(out))
  out <- c(out, set[missingmaps])


  out <- as.data.frame(out)

  
  ## Set shape to pass to shape and rendering methods
  class(out) <- c(.getSonlayerShape(x, sonlayernum),  "data.frame")




  ## Any additional score processing done by shape-specific methods to
  ## scorePreprocessor. NOTE: all scorePreprocessor methods must
  ## calculate length of sonification
  out <- scorePreprocessor(out)

  if(is.null(attributes(out)$length))
    stop("scorePreprocessor.", .getSonlayerShape(x, sonlayernum),
         " failed to add required 'length' attribute.")

  return(out)
}


##' @rdname internaldf
##' @param remove.null Logical indicating whether to remove missing
##' mappings from the returned list of mappings. .getSonlayerScore
##' calls this with TRUE to avoid cluttering calculations; but
##' .checkSonify calls this with FALSE since it bases its approach on
##' having the null slots in.
.getSonlayerMappings <- function(x, sonlayernum, remove.null = TRUE) {
  ## x: a sonify object, returns the current aesthetic mappings as a
  ##  named list. This assign aesthetic mappings based on sonlayer

  if(sonlayernum > length(x$sonlayers))
    stop(paste("There is no sonlayer", sonlayernum))
  
  shape <- .getSonlayerShape(x, sonlayernum)
  outmap <- x$mapping # use top-level mapping as starting point
  sonlayermap <- (x$sonlayers[[sonlayernum]])$mapping
    
  ## If there are any sonlayer mappings, override toplevel 
  if(!is.null(sonlayermap)) {
    for(i in names(sonlayermap)) {
      if(!is.null(sonlayermap[[i]]))
        outmap[[i]] <- sonlayermap[[i]]
    }
  }

  ## If remove.null, remove list elements whose value is NULL
  if(remove.null)
    outmap <- removenull(outmap)    
  
  return(outmap)
}

##' @rdname internaldf
##' @param transform A logical indicating whether to perform the
##' given statistical transformation (stat) to the layer.
.getSonlayerData <- function(x, sonlayernum, transform = TRUE) {
  ## x: a sonify object, returns the current data to be sonified,
  ## after applying statistic if defined.

  if(sonlayernum > length(x$sonlayers))
    stop(paste("There is no sonlayer", sonlayernum))

  if(!is.null(x$sonlayers[[sonlayernum]]$data)){
    outdata <- x$sonlayers[[sonlayernum]]$data} else {
      outdata <- x$data
    }
  
  if(transform) {
    stat <- .getSonlayerStat(x, sonlayernum)
    if(!is.null(stat))
      outdata <- stat(outdata)
  }
  return(outdata)
}

##' @rdname internaldf
.getSonlayerSettings <- function(x, sonlayernum, remove.null = TRUE) {
  ## Get sonlayer SETTINGS (as opposed to mappings)
  shape <- .getSonlayerShape(x, sonlayernum)
  outset <- .getDefaultSettings(shape) # use default settings as starting point

  ## The shape_params that match the possible sound params are the
  ## settings we want.
  params <- .getSoundParams(shape)$param
  sonlayersettings <-(x$sonlayers[[sonlayernum]])$shape$shape_params
  sonlayersettings <- sonlayersettings[names(sonlayersettings) %in% params]

  ## Override defaults by given params
  if(!is.null(sonlayersettings)) {
    for(i in names(sonlayersettings)) {
      if(!is.null(sonlayersettings[[i]]))
        outset[[i]] <- sonlayersettings[[i]]
    }
  }

    ## If remove.null, remove list elements whose value is NULL
  if(remove.null) 
    outset <- removenull(outset)    
    
  return(outset)
}

##' @rdname internaldf
##' @section Not yet implemented: .getSonlayerStat currently just returns
##' NULL. It is intended that it will eventually return a function
##' that .getSonlayerData can use to transform a data.frame, or NULL
##' if no tranformation is requested. This function (and its only call
##' in .getSonlayerData) will likely change soon.
.getSonlayerStat <- function(x, sonlayernum) NULL

##' @rdname internaldf
.getSonlayerShape <- function(x, sonlayernum) {
  x$sonlayers[[sonlayernum]]$shape$shape
}

##' @rdname internaldf
##' @param param The sound parameter
##' @param column The data.frame column (vector) to be rescaled
.rescaleDataByParam <- function(x, param, column) {
  ## If the parameter has a scaling associated with it in the sonify
  ## object, apply it; otherwise return the column verbatim
  if(!is.null(x$scales[[param]]))
    scale <- x$scales[[param]]
  else {
    ## Figure out shapes in order of their unique appearance in sonlayers
    shapes <- unique(sapply(1:length(x$sonlayers),
                            function(y) .getSonlayerShape(x, y)))
    scale <- .getDefaultScalingByParam(param, shapes)
  }

  ##Apply function
  column <- scale$scaling.function(column, scale$min, scale$max)

  return(column)
}

##' @rdname internaldf
##' @param shapes A character vector of names of shapes included in
##' the \code{sonify} object
.getDefaultScalingByParam <- function(param, shapes) {
  ## Get the data.frame of all soundparameters
  soundparams <- .getSoundParams(shapes)

  ## We want to recall sound parameters IN THE ORDER THAT SHAPES
  ## APPEAR IN THE SONLAYERS. Thus if there is a conflict between
  ## defaults, the earliest shape to appear in the sonlayers takes
  ## precedence. This is arbitrary behavior, but it's worth doing and
  ## documenting.
  soundparams$shape <- ordered(soundparams$shape, levels=shapes)
  soundparams <- soundparams[order(soundparams$shape),]
  lookup <- soundparams[soundparams$param %in% param,][1,]
  
  lookup <- as.character(lookup$shape) # since it's a factor
  return(.getShapeDef(lookup)$params[[param]]$defaultScaling)
}

##' @rdname internaldf
##' @param shapename The name of the shape to get the defaults for
.getDefaultShapeOptions <- function(shapename) {
  .getShapeDef(shapename)$options
}

##' @rdname internaldf
.getSonlayerShapeOptions <- function(x, sonlayernum) {
  shape <- .getSonlayerShape(x, sonlayernum)
  out <- .getDefaultShapeOptions(shape)
  supplied <- x$sonlayers[[sonlayernum]]$shape$shape_params

  for(i in names(supplied))
    out[[i]] <- supplied[[i]]

  return(out)
}
                


##' @rdname internaldf
##' @param sonlayerscore The score generated for a specific
##' \code{sonlayer} by \code{.getSonlayerScore()}
##' @note all scorePreprocessor methods must calculate length of
##' sonification and return that as an attribute \code{length} of the
##' data frame.
scorePreprocessor <- function(sonlayerscore) UseMethod("scorePreprocessor")

  
removenull <- function(x) {
  out <- Filter(function(y) !is.null(y), x)
  newnames <- intersect(names(out), names(x))
  mostattributes(out) <- attributes(x)
  names(out) <- newnames
  out
}
