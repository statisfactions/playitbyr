##' Internal functions to generate output \code{data.frame} from a
##' \code{sonify} object.
##' 
##' These functions, together, are used to generate a standardized
##' \code{data.frame}, rather like a Csound score, that is then used
##' by whatever method renders the sounds (currently only rendering in
##' R as sine waves is supported).
##'
##' @note These functions (particularly .SonlayerScore) assume that
##' every shape has a single output row for each input data row after
##' applying statistical tranformations
##' 
##' @rdname getScore
##' @aliases .getScore
##' @param x A \code{sonify} object
##' @return The end product of all this is a list of \code{data.frame}
##' objects that is used as an input to the rendering process
##' @keywords internal
##' @export
.getScore <- function(x) {
  
  ## Returns a data.frame score that the render method
  ## uses to actually create the sound for the sonification


  .checkSonify(x)

  ## get scales; train if faceting
  if(!is.null(x$sonfacet) && x$sonfacet$scales == "fixed") {
    scale <- .fixFacetScales(x) 
  } else 
    scale <- .getScales(x)
  
  ## Create a list of facet scores for each sonlayer and put together in list
  scorefacet <- lapply(1:length(x$sonlayers),
                  function(layernum) .getSonlayerScore(x, layernum, scale))

  ## get lengths of facets for each layer and put in data.frame
  lengthfacets <- as.data.frame(lapply(scorefacet, function(y)
                                       sapply(y, function(z) attributes(z)$length)))
  
  lengths <- sapply(1:nrow(lengthfacets), function(y) max(lengthfacets[y,]))

  score <- lapply(scorefacet, function(y) {
    if(length(y) > 1) {
      out <- facetjoin(y, pause = x$sonfacet$pause, lengths = lengths)
      class(out) <- c(class(y), "data.frame")
      attr(out, "shape_params") <- attributes(y)$shape_params
    } else
      out <- y[[1]]
    out
  })

    
                  
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

## @rdname internaldf
.getSonlayerScore <- function(x, sonlayernum, scale) {

  data <- .getSonlayerData(x, sonlayernum, transform = TRUE)
  map <- .getSonlayerMappings(x, sonlayernum, remove.null = TRUE) 
  set <- .getSonlayerSettings(x, sonlayernum, remove.null = TRUE)
  shape <- .getSonlayerShape(x, sonlayernum)
  opts <- .getSonlayerShapeOptions(x, sonlayernum)

  ## Exclude data outside max min on any parameter
  datal <- lapply(names(map), function(param) {
    column <- .getColumn(map, param, data)
    lims <- scale[[param]]$limits
    if(!is.null(lims))
      column[column < min(lims) | column > max(lims)] <- NA
    return(column)
  })

  dataclean <- as.data.frame(removenull(datal))
  names(dataclean) <- unlist(map)

  if(!is.null(x$sonfacet)) {
    if(x$sonfacet$facet %in% names(data)) {
      datal <- split(dataclean, data[[x$sonfacet$facet]], drop = TRUE)
      scorel <- lapply(datal, function(d)
                     .getSonlayerScoreFacet(map, set, shape, d, scale, opts))
  }} else {
    scorel <- list(.getSonlayerScoreFacet(map, set, shape, dataclean, scale, opts))
  }

  ## Set shape to pass to shape and rendering methods
  class(scorel) <- c(shape)

  ## Add shape options, if any, to pass to rendering methods
  attr(scorel, "shape_params") <- .getSonlayerShapeOptions(x, sonlayernum)
  ## TODO subtract out sound params from this function
  scorel
}

## @rdname internaldf
.getSonlayerScoreFacet <- function(map, set, shape, data, scale, opts) {
  ## Returns an output data.frame with all the information needed to
  ## render the sonlayernum-th sonlayer of x.  The output is in a
  ## format rather similar to a Csound score.

  n <- nrow(data) # length of output score

  if(n == 0)
    ## if no data here, skip this facet
    return(NULL)

  ## Create output score:
  ## If map gives data.frame column, coerce to numeric and rescale that column.
  ## If map gives anything else, create new variable and then rescale column.
  out <- lapply(names(map), function(param) {
    column <- .getColumn(map, param, data)
    if(!is.numeric(column))
      column <- as.numeric(as.factor(column))
    return(.rescaleDataByParam(scale, param, column))
  })

  ## add on settings for params that do not yet have columns
  names(out) <- names(map)
  missingmaps <- setdiff(names(set), names(out))
  out <- c(out, set[missingmaps])

  out <- as.data.frame(out)
  
  ## Set shape to pass to shape and rendering methods
  class(out) <- c(shape,  "data.frame")

  ## Any additional score processing done by shape-specific methods to
  ## scorePreprocessor. NOTE: all scorePreprocessor methods must
  ## calculate length of sonification
  out <- scorePreprocessor(out, opts)

  if(is.null(attributes(out)$length))
    stop("scorePreprocessor.", shape,
         " failed to add required 'length' attribute.")

  return(out)
}

.getColumn <- function(map, param, data) {
  ## if the length if map
  if(length(map[[param]]) == 1 && map[[param]] %in% names(data))
    column <- data[[as.character(map[[param]])]]
  else
    column <- eval(map[[param]])
  if(!is.numeric(column))
    column <- as.numeric(factor(column))
  return(column)
}


## @rdname internaldf
## @param remove.null Logical indicating whether to remove missing
## mappings from the returned list of mappings. .getSonlayerScore
## calls this with TRUE to avoid cluttering calculations; but
## .checkSonify calls this with FALSE since it bases its approach on
## having the null slots in.
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

## @rdname internaldf
## @param transform A logical indicating whether to perform the
## given statistical transformation (stat) to the layer.
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

## @rdname internaldf
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

## @rdname internaldf
## @section Not yet implemented: .getSonlayerStat currently just returns
## NULL. It is intended that it will eventually return a function
## that .getSonlayerData can use to transform a data.frame, or NULL
## if no tranformation is requested. This function (and its only call
## in .getSonlayerData) will likely change soon.
.getSonlayerStat <- function(x, sonlayernum) NULL

## @rdname internaldf
.getSonlayerShape <- function(x, sonlayernum) {
  x$sonlayers[[sonlayernum]]$shape$shape
}

## @rdname internaldf
## @param param The sound parameter
## @param column The data.frame column (vector) to be rescaled
.rescaleDataByParam <- function(scale, param, column) {
  ##Apply function
  outcolumn <- scale[[param]]$scaling.function(column, limits = scale[[param]]$limits, soundlimits = scale[[param]]$soundlimits)

  return(outcolumn)
}

## @rdname internaldf
.getScales <- function(x) {
  shapes <- unique(sapply(1:length(x$sonlayers),
                            function(y) .getSonlayerShape(x, y)))
  scale <- .getDefaultScalings(shapes)
  scale[names(x$scales)] <- x$scales
  scale
}

.fixFacetScales <- function(x) {
  ## get all sonlayer mappings for each of these
  scales <- .getScales(x)
  all <- lapply(1:length(x$sonlayers), function(i) {
    map <- .getSonlayerMappings(x, i)
    data <- .getSonlayerData(x, i)
    extremes <- lapply(names(map), function(param) {
      column <- .getColumn(map, param, data)
      return(param = c(min(column), max(column)))
    })
    names(extremes) <- names(map)
    extremes <- do.call(rbind,extremes)
  })
  all <- do.call(rbind,all)
  nm <- row.names(all)
  row.names(all) <- NULL
  all <- as.data.frame(all)
  names(all) <- c("min", "max")
  all$map <- as.character(nm)
  lapply(split(all, all$map), function(y) {
    scales[[y$map[1]]]$limits <- c(min(y$min), max(y$max))
    scales[[y$map[1]]]
  })
}



## @rdname internaldf
## @param shapes A character vector of names of shapes included in
## the \code{sonify} object
.getDefaultScalings <- function(shapes) {
  ## Get the data.frame of all soundparameters
  soundparams <- .getSoundParams(shapes)

  ## We want to recall sound parameters IN THE ORDER THAT SHAPES
  ## APPEAR IN THE SONLAYERS. Thus if there is a conflict between
  ## defaults, the earliest shape to appear in the sonlayers takes
  ## precedence. This is arbitrary behavior, but it's worth doing and
  ## documenting.
  soundparams$shape <- ordered(soundparams$shape, levels=shapes)
  soundparams <- soundparams[order(soundparams$shape),]
  soundparams <- soundparams[!duplicated(soundparams$param),]

  out <- lapply(1:nrow(soundparams), function(y) {
    x <- soundparams[y, ]
    scaleparam <- .getShapeDef(as.character(x[["shape"]]))
    scaleparam <- scaleparam$params[[x[["param"]]]]$defaultScaling
    names(scaleparam) <- c("limits", "soundlimits", "scaling.function")
    scaleparam
   })
  names(out) <- soundparams$param
  class(out) <- "sonscaling"
  return(out)
}

## @rdname internaldf
## @param shapename The name of the shape to get the defaults for
.getDefaultShapeOptions <- function(shapename) {
  .getShapeDef(shapename)$options
}

## @rdname internaldf
.getSonlayerShapeOptions <- function(x, sonlayernum) {
  shape <- .getSonlayerShape(x, sonlayernum)
  out <- .getDefaultShapeOptions(shape)
  supplied <- x$sonlayers[[sonlayernum]]$shape$shape_params

  for(i in names(supplied))
    out[[i]] <- supplied[[i]]

  return(out)
}
                

##' Generic method for preprocessing score layers
##'
##' Generic to do extra needed processing and calulation for a
##' specific sonlayer shape.
##' 
##' @param sonlayerscore The score generated for a specific
##' \code{sonlayer} by \code{.getSonlayerScore()}
##' @param opts The options passed as shape parameters in a \code{sonlayer}
##' @param \dots Other things to pass to scorePreprocessor. Currently ignored.
##' @return A score after shape-specific processing has been completed.
##' @note all scorePreprocessor methods must calculate length of
##' sonification and return that as an attribute \code{length} of the
##' data frame.
##' @keywords internal
scorePreprocessor <- function(sonlayerscore, opts, ...) UseMethod("scorePreprocessor")

  
removenull <- function(x) {
  out <- Filter(function(y) !is.null(y), x)
  newnames <- intersect(names(out), names(x))
  mostattributes(out) <- attributes(x)
  names(out) <- newnames
  out
}

facetjoin <- function(scorel, pause, lengths = NULL) {
  ## if lengths is null, assume that we are getting the lengths from
  ## the elements of scorel, and (below) adding on that attribute
  ## ourselves.
  if(is.null(lengths))
    facetend <- cumsum(sapply(scorel, function(s) attr(s, "length")))
  else
    facetend <- cumsum(lengths)

  nl <- length(scorel)

  facetend <- facetend + (0:(nl-1))*pause
  scorelshift <- lapply(1:nl, function(fn) {
      outsc <- scorel[[fn]]
      if(fn > 1) 
        outsc$start <- outsc$start + facetend[fn - 1] + pause
      return(outsc)
    })
  score <- do.call(rbind, scorelshift)

  attr(score, "length") <- facetend[nl]

  return(score)
}

