##' Internal functions to generate output \code{data.frame} from a
##' \code{sonify} object.
##' 
##' These functions, together, are used to generate a standardized
##' \code{data.frame}, rather like a Csound score, that is then used by
##' whatever method renders the sounds (currently only rendering in R as sine
##' waves is supported).
##'
##' @note These functions (particularly .getSonlayerScore) assume that every
##' shape has a single output row for each input data row after
##' applying statistical tranformations
##' 
##' @rdname internaldf
##' @name internaldf
##' @aliases .getScore .getSonlayerMappings .getSonlayerData
##' .getSonlayerScore
##' @param x A \code{sonify} object
##' @param sonlayernum The layer number of the \code{sonify} object. This is a
##' placeholder for future implementation of a layering functionality (modeled
##' after \code{ggplot2} objects).
##' @return The end product of all this is a \code{data.frame} object that is
##' used as an input to the rendering process
##' @keywords internal
##' 
.getScore <- function(x) {
  ## Returns a data.frame score that the render method
  ## uses to actually create the sound for the sonification

  ## Check to make sure there are layers to render
  if(is.null(x$sonlayers))
    stop("Cannot render sound without any sonlayers.")

  ## Create a score for each sonlayer and concatenate all scores into
  ## a data.frame
  score <- lapply(1:length(x$sonlayers),
                  function(layernum) .getSonlayerScore(x, layernum))
  return(score)
}

##' @rdname internaldf
.getSonlayerScore <- function(x, sonlayernum) {
  ## Returns an output data.frame with all the information needed to
  ## render the sonlayernum-th sonlayer of x.  The output is in a
  ## format rather similar to a Csound score.

  
  ## Get mappings and stat-transformed data
  map <- .getSonlayerMappings(x, sonlayernum, remove.null = TRUE) 
  data <- .getSonlayerData(x, sonlayernum, transform = TRUE)

  n <- nrow(data) # length of output score

  ## Create output score: If given aesthetic mapping is a numeric
  ## constant, that sonic parameter should be that
  ## constant. Otherwise, if data.frame column is given, rescale that
  ## that data.frame column (based on the given scaling)
  out <- lapply(names(map), function(param) {
    if(is.numeric(map[[param]]))
      column <- rep(map[[param]], n)
    else {
      column <- data[[map[[param]]]]
      column <- .rescaleDataByParam(x, param, column)
    }
    return(column)
  })      
  names(out) <- names(map)
  out <- as.data.frame(out)


  ## We need to transform the "tempo" or "time" data into actual start
  ## times and tranform the durations accordingly

  if("tempo" %in% names(out)) {
    ## If tempo is provided, convert tempo data into start times, sort
    ## score, and scale durations in relation to beat
    beatlength <- 60/out$tempo
    out$tempo <- NULL
    out$start <- c(0, cumsum(beatlength[-n]))
    total <- out$start[n]+ beatlength[n] #used to calculate dur
  } else if("time" %in% names(out)) {
    ## Otherwise, just rename 'time' to 'start' and sort by that.
    out$start <- out$time
    out$time <- NULL
    out <- out[order(out$start),]
    total <- out$start[n] + mean(out$start[-1] - out$start[-n]) #used to calculate dur
  }

  ## Scale durations by total time divided by number of notes
  out$dur <- (out$dur) * (total/n) 
  ## (NOTE: this is somewhat questionable whether this is the right
  ## thing to do; it's a little arbitrary and makes scaling duration
  ## less intuitively related to what's specified in x$scaling.  I
  ## have chosen to do it this way b/c it means you can easily set a
  ## different scaling for the time and duration will automagically
  ## scale down without having to set it separately, which seems
  ## annoying.)

  ## Set shape to pass to rendering method
  attr(out, "shape") <- .getSonlayerShape(x, sonlayernum)

  return(out)
}


##' @rdname internaldf
##' @param remove.null Logical indicating whether to remove missing
##' mappings from the returned list of mappings. .getSonlayerScore
##' calls this with TRUE to avoid cluttering calculations; but
##' checkSonify calls this with FALSE since it bases its approach on
##' having the null slots in.
.getSonlayerMappings <- function(x, sonlayernum, remove.null = TRUE) {
  ## x: a sonify object, returns the current aesthetic mappings as a
  ##  named list This assign aesthetic mappings based on sonlayer, and
  ##  on default if sonlayer mapping not present.

  if(sonlayernum > length(x$sonlayers))
    stop(paste("There is no sonlayer", sonlayernum))

  outmap <- x$mapping # use default aesthetic mappings as starting point
  sonlayermap <- x$sonlayers[[sonlayernum]]$mapping

  ## If there are any sonlayer mappings, override all default aesthetic
  ## mappings with the sonlayer mappings
  if(!is.null(sonlayermap)) {
    for(i in names(sonlayermap)) {
      if(!is.null(sonlayermap[[i]]))
        outmap[[i]] <- sonlayermap[[i]]
    }
  }

  if(remove.null) {
    ## If remove.null, remove list elements whose value is NULL
    i <- 1
    while(i <= length(outmap)) {
      ## See R Faq "How can I set components of a list to NULL" for
      ## more on how this works... looks odd, but it's good
      if(is.null(outmap[[i]])) outmap[[i]] <- NULL
      else i <- i +1
    }
  }
  return(outmap)
}

##' @rdname internaldf
##' @param transform A logical indicating whether to perform the
##' given statistical transformation (stat) to the layer.
.getSonlayerData <- function(x, sonlayernum, transform = TRUE) {
  ## x: a sonify object, returns the current data to be sonified, after applying 

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
##' @section Not yet implemented: .getStat currently just returns
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
  x$scales[[param]]$scaling.function(column, x$scales[[param]]$min, x$scales[[param]]$max)
}

