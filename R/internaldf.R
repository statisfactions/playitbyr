.getScore <- function(s) {
  ## Returns a data.frame score that the render method
  ## uses to actually create the sound for the sonification

  ## s is a "sonify" object containing sonlayers

  ## Check to make sure there are layers to render
  if(is.null(s$sonlayers))
    stop("Cannot render sound without any sonlayers.")

  ## Create a score for each sonlayer and concatenate all scores into a data.frame
  score <- do.call(rbind, lapply(1:length(s$sonlayers),
                                 function(x) .getSonlayerScore(s, x)))
  return(score)
}

.getSonlayerScore <- function(x, sonlayernum) {
  ## Returns an output data.frame with all the information needed to render
  ## the sonlayernum-th sonlayer of x.
  ## The output is in a format rather similar to a Csound score.

  ## Create initial data frame
  n <- nrow(x$data) # for notes shape, every row of df is a note
  sonlayer <- rep(sonlayernum, n) 
  out <- data.frame(sonlayer)

  ## Get mappings and data
  map <- .getSonlayerMappings(x, sonlayernum) 
  data <- .getSonlayerData(x, sonlayernum)
  
  ## If tempo or time is NULL, remove it from the sound parameters we're
  ## looking at
  if(is.null(map$tempo)) sound.params <- setdiff(names(map), "tempo")
  if(is.null(map$time)) sound.params <- setdiff(names(map), "time")
  
  ## For each sound parameter, create a column of the output data.frame corresponding with it
  for(j in sound.params) {
    map.value <- map[[j]]
    if(map.value %in% names(data)) {
      ## If the mapping value for sound parameter j matches a column name
      ## of the data, we scale with the scaling function, minimum, and maximum
      ## provided for sound parameter j
      jscaling <- x$scales[[j]]
      out[[j]] <- jscaling$scaling.function(data[[map.value]], jscaling$min, jscaling$max)
    } else {
    ## Otherwise,  we take it as a constant and copy it over the whole output data.frame
      out[[j]] <- map.value
    }
  }

  ## Creating start times from 'tempo' or 'time' information:
  if(!is.null(map$tempo)) {
    ## If tempo is provided, convert tempo data
    ## into start times and scale durations in relation to beat
    beatlength <- 60/out$tempo
    out$start <- c(0, cumsum(beatlength[-n]))
    total <- out$start[n]+ beatlength[n] #used to calculate duration below
  } else if(!is.null(map$time)) {
    ## Otherwise, 
    out$start <- out$time
    out <- out[order(out$start),]
    total <- out$start[n] + mean(out$start[-1] - out$start[-n]) #used to calculate duration below
  }

  ## Scale durations by total time divided by number of notes
  out$dur <- (out$dur) * (total/n) 

  ## (NOTE: this is somewhat questionable whether this is the right
  ## thing to do; it's a little arbitrary and makes scaling duration
  ## less intuitively related to what's specified in x$scaling.
  ## I have chosen to do it this way b/c it means you can easily set a different
  ## scaling for the time and duration will automagically scale down without
  ## having to set it separately, which seems annoying.)

  
  
  ## Return only the standard columns expected by the render methods,
  ## since we've introduced a bunch of other crap into the picture.
  out[,c("sonlayer", "start", "dur", "pitch", "vol", "pan", "timbre")] 
}



.getSonlayerMappings <- function(x, sonlayernum) {
  ## x: a sonify object, returns the current mappings as a named list
  ## 1. assign mapping based on sonlayer, and on default if sonlayer mapping not present
  if(sonlayernum > length(x$sonlayers)) stop(paste("There is no sonlayer", sonlayernum))

  sonlayermap <- x$sonlayers[[sonlayernum]]$mapping
  if(!is.null(sonlayermap)) {
    for(i in names(sonlayermap)) {
      if(!is.null(sonlayermap[[i]]))
        x$mapping[[i]] <- sonlayermap[[i]]
    }
  }
  return(x$mapping)
}

.getSonlayerData <- function(x, sonlayernum) {
  ## x: a sonify object, returns the current data to be sonified
  if(sonlayernum > length(x$sonlayers)) stop(paste("There is no sonlayer", sonlayernum))

  if(!is.null(x$sonlayers[[sonlayernum]]$data)){
    return(x$sonlayers[[sonlayernum]]$data)} else {return(x$data)}
} 
