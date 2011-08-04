## TODO check checkSonify's new functionality
## and sonlayer and all the new stuff...

sonify <- function(data=NULL, mapping=sonaes(), scales=sonscaling()) {
  ## Creates a \code{sonify} object, which is a list containing the \code{data.frame}
  ## to be sonified, the mappings of data to sound parameters, the scaling
  ## of parameters, and additional options.
  

####################TEMPORARY####################
  ## This code is a temporary hack in place of future functionality
  ## TODO Make as arguments in sonify() once there are other options
  sonlayers <- shape_notes()
  rendering <- "audio"
####################END##########################

  .checkRendering(rendering)
  dataname <- deparse(substitute(data)) # Used by summary.sonify()
  
  s <- list(data, dataname, mapping, rendering, scales, sonlayers) 
  names(s) <- c("data", "dataname", "mapping", "rendering", "scales", "sonlayers") 
  class(s) <- c(rendering, "sonify")    # The class of rendering determines the
                                        # function called to render s
  .checkData(s)
  s
}

sonlayer <- function(shape="notes", shape_params=NULL, stat=NULL,
                     stat_params=NULL, data=NULL, mapping=NULL) {
  ## TODO Let's only make live the parts of this we're actually supporting
  ## TODO man page
  ## TODO add to namespace

  dataname <- deparse(substitute(data)) # Used by summary.sonify()
  l <- list(list(shape, shape_params), list(stat, stat_params), data, dataname, mapping)
  
  names(l) <- c("shape", "stat", "data", "dataname", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- c("sonlayer")

  .checkData(l)    
  l
}

shape_notes <- function(...) sonlayer("notes",...)
## TODO add to namespace  
## Convenience function for the only supported layer type, notes.

sonaes <- function(time=0, pitch=8, dur=2, vol=1, pan=0.5, tempo=NULL, timbre="sine") {
  ##Similar to ggplot2 "aes", for generating mappings of data to sound.
  ## 'sonaes' objects are lists and are used as the top-level 'mapping' of sonify objects
  
  if(!missing(time) && !missing(tempo))
    stop("Only one of 'time' or 'tempo' can be provided.")

  given <-  as.list(match.call()[-1])
  ## Deparse any unquoted data.frame columns given as args
  given <- lapply(given, function(x) {
                  x <- ifelse(is.symbol(x), deparse(x), x)
                  if(!is.null(x)) attr(x, "default") <- FALSE
                  return(x)})
  son <- lapply(formals(), function(x){
                if(!is.null(x)) attr(x, "default") <- TRUE
                return(x)})
  son[match(names(given), names(son))] <- given

  ## Auto-adjust for one of time, tempo being not given
  if(missing(time) && !missing(tempo))
    son["time"] <- list(NULL)
  if(!missing(time) && missing(tempo))
    son["tempo"] <- list(NULL)

  ## Check validity
  if(!is.null(son$time) && son$time < 0)
    stop("time must be greater than 0.")
  if(!is.null(son$tempo) && (son$tempo<=0))
    stop("tempo must be greater than 0 (bpm)")
  if(son$dur<0)
    stop("dur cannot be negative")
  if(((son$vol<0) || (son$vol>1)))
    stop("vol must be between 0 and 1.")
  if((son$pan<0) || (son$pan>1))
    stop("pan must be between 0 and 1.")
  if(son$timbre != "sine")
    stop("'sine' is the only supported timbre right now")  

  class(son) <- c("sonaes")
  son
}

"+.sonify" <- function(x, y) {
  if("sonlayer" %in% class(y)) {
    ## adds sonlayer
    if(is.null(x$sonlayers)) {
      x$sonlayers[[1]] <- y
    } else {x$sonlayers <- c(x$sonlayers, list(y))}
  } else if("sonscaling" %in% class(y)) {
    ## adds to or overrides scale
    for(i in names(x$scales)) {
      if(!is.null(y[[i]])) {
        if(!attr(y[[i]], "default"))
          x$scales[[i]] <- y[[i]]
      }
    }
  } else if("sonaes" %in% class(y)) {
    for(i in names(x$mapping)) {
      if(!is.null(y[[i]])) {
        if(!attr(y[[i]], "default"))
          x$mapping[[i]] <- y[[i]]
      }
    }
    if(is.null(y$time) && !is.null(x$mapping$tempo))
      x$mapping["time"] <- list(NULL)
    if(is.null(y$tempo) & !is.null(x$mapping$time))
      x$mapping["tempo"] <- list(NULL)
  } else if("sonrendering" %in% class(y)) {
    x$rendering <- y
    class(x) <- c(y, class(x)[-1])
  } else {stop("'+' operator not supported for this operation.")}
  x
}         

checkSonify <- function(x) {
  ## TODO note data check on man page

  xname <- deparse(substitute(x))
  if(!("sonify" %in% class(x)))
    stop("'",xname,"' is not a 'sonify' object.")
  
  ## Do any layers of x contain data?
  layers.null <- all(sapply(x$sonlayers, function(y) is.null(y$data)))
  if(is.null(x$data) & layers.null)
    stop("No data.frame provided for sonification. See ?sonify.")
  
  ## Checks that correct mapping slots are filled  
  map <- .getMappings(x, 1)
  if(!xor(is.null(map$time), is.null(map$tempo)))
    stop("Either 'time' or 'tempo' must be set, but not both. See ?sonaes.\n\n",
         xname, "$mapping$time:  ", as.character(x$mapping$time), "\n",
         xname, "$mapping$tempo: ", as.character(x$mapping$tempo), "\n")
  if(is.null(map$tempo)) mapnames <- setdiff(names(map), c("tempo", "timbre"))
  if(is.null(map$time)) mapnames <- setdiff(names(map), c("time", "timbre"))
  
  nullmaps <- sapply(mapnames, function(y) {
    if(is.null(map[[y]])) return(y) else return(NA)})
  nullmaps <- na.omit(as.vector(nullmaps, "character"))
  nullmaps

  if(length(nullmaps)>0)
    stop("These sonic parameters need to be set to a value or \n",
         "mapped to a data.column (see ?sonaes):\n",
         paste(nullmaps, collapse=", "), ".")

  ## Checks that any non-numeric mappings correspond to a numeric
  ## data column  
  nonnumericmaps <- sapply(mapnames, function(y) {
    if(!is.numeric(map[[y]])) return((y=map[[y]])) else return(NA)})
  nonnumericmapnames <- names(nonnumericmaps)[!is.na(nonnumericmaps)]
  nonnumericmaps <- na.omit(as.vector(nonnumericmaps, "character"))
  if(length(nonnumericmaps)==0)
    stop("No data columns selected as mappings. You need to set\n",
         "at least one data column to be mapped with sonaes().")
  names(nonnumericmaps) <- nonnumericmapnames
  unmatched <- nonnumericmaps[!(nonnumericmaps %in% names(x$data))]
  if(length(unmatched)>1)
    stop("These sonic parameters are set as nonnumeric with sonaes(),\n",
         "so they are assumed to be columns of the data.frame for\n",
         "sonification. BUT, they do not match any columns in the\n",
         "given data.frame:\n\n",
         paste(names(unmatched), ": ", unmatched, sep="", collapse="\n"), "\n\n",
         "data.frame column names:\n", paste(names(x$data), collapse="\n"))

  datatest <- sapply(na.omit(nonnumericmaps), function(y) {
    is.numeric(x$data[,y])})
  if(any(!datatest))
    stop("Non-numeric columns of the dataset cannot be used in a mapping.\n\n",
         "These columns are non-numeric and are used as mappings:\n",
         paste(nonnumericmaps[!datatest], collapse="\n"))

  ## Check that all non-numeric mappings have an associated scaling
  scalesnull <- sapply(nonnumericmapnames, function(y) {
    is.null(x$scales[[y]])})
  if(any(scalesnull))
    stop("Every mapping of a sonic parameter to a data column must also\n",
         "have an associated scale.\n\n",
         "Sonic parameters with missing scales:\n",
         paste(nonnumericmapnames[scalesnull], collapse="\n"))
}                               

render <- function(x) UseMethod("render")

print.sonify <- function(x, ...) {
  checkSonify(x)
  out <- render(x)
  playAudioRendering(out)
}

summary.sonify <- function(object, ...) {
  mins <- as.vector(lapply(object$scales, function(y) y$min), "character")
  maxs <- as.vector(lapply(object$scales, function(y) y$max), "character")
  firstspaces <- sapply(names(object$scales), function(y) paste(rep(" ",17 - nchar(y)), collapse=""))
  secondspaces <- sapply(mins, function(y) paste(rep(" ",8 - nchar(y)), collapse=""))

  cat((paste("Summary of sonify object '", deparse(substitute(object)), "':\n\n", sep="")))

  cat("The data to be sonified:\n", paste("$dataname \n",
                                          object$dataname, "\n\n"))
  cat("Matchup of sonic values to data columns or constants:\n",
      "      $mapping")
  cat("           Column or Value\n"  )
  cat("--------------------------------------------\n")
  cat(paste("        $", names(object$mapping), " ",
            firstspaces,
            as.vector(object$mapping, "character"),
            sep="", collapse="\n"), "\n\n")

  cat("Desired min/max for sonic parameters:\n",
      "      $scales")
  cat("           Min      Max\n")
  cat("--------------------------------------------\n")
  cat(paste("        $", names(object$scales),
            firstspaces, mins, secondspaces, maxs,
            collapse="\n", sep=""))
  cat("\n")

}   


`%+%` <- function(x, y) {
  ##Another possible ggplot2 confusion/conflict
  ##Replace data.frame in x (a sonify object)
  ##with y (data.frame)

  ## This function does not check whether the y's names
  ## match the names in x$mapping, but this is checked
  ## before rendering by checkSonify
  
  x$data <- y
  .checkData(x)
  x$dataname <- deparse(substitute(y))
  x
}

.dfNotes <- function(s) {
  ## s is a "sonify" object containing all notes sonlayers
  ## This function renders the "notes" shape

  if(is.null(s$sonlayers)) stop("Cannot render sound without any sonlayers.")

  notes <- do.call(rbind, lapply(1:length(s$sonlayers), function(x) .getNotes(s, x)))
  notes
}




.getMappings <- function(x, sonlayernum) {
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

.getData <- function(x, sonlayernum) {
  ## x: a sonify object, returns the current data as a data.frame
  if(sonlayernum > length(x$sonlayers)) stop(paste("There is no sonlayer", sonlayernum))

  if(!is.null(x$sonlayers[[sonlayernum]]$data)){
    return(x$sonlayers[[sonlayernum]]$data)} else {return(x$data)}
} 

.getNotes <- function(x, sonlayernum) {
  ## Create the notes for the given sonlayer into Csound score style format
  n <- nrow(x$data) # for notes shape, every row of df is a note
  sonlayer <- rep(sonlayernum, n)
  curnotes <- data.frame(sonlayer)
  
  map <- .getMappings(x, sonlayernum)
  
  if(is.null(map$tempo)) mapnames <- setdiff(names(map), "tempo")
  if(is.null(map$time)) mapnames <- setdiff(names(map), "time")
  
  data <- .getData(x, sonlayernum)

  for(j in mapnames) {
    if(map[[j]] %in% names(data)) {

      ##TODO: some sort of check here before simply assigning variables
      curnotes[[j]] <- x$scales[[j]]$scaling.function(data[[ (map[[j]]) ]], x$scales[[j]]$min, x$scales[[j]]$max)
    } else {curnotes[[j]] <- map[[j]]}
  }

  ## convert tempo data into start times and scale durations in relation to beat
  if(!is.null(map$tempo)) {
    beatlength <- 60/curnotes$tempo
    curnotes$start <- c(0, cumsum(beatlength[-n]))
    total <- curnotes$start[n]+ beatlength[n]

  } else if(!is.null(map$time)) {
    curnotes$start <- curnotes$time
    curnotes <- curnotes[order(curnotes$start),]
    
    total <- curnotes$start[n] + mean(curnotes$start[-1] - curnotes$start[-n]) 
  }
  curnotes$dur <- curnotes$dur*(total/n)
  curnotes$sonlayer <- factor(curnotes$sonlayer)
  curnotes[,c("sonlayer", "start", "dur", "pitch", "vol", "pan", "timbre")]
}

.checkRendering <- function(x) {
  ## Checks whether input string x is a valid rendering type
  ## This is a dopey function for now, but is likely to become
  ## more complex as more rendering types are supported,
  ## some of which may even be machine or architecture dependent.
  ## This is called by 'rendering()' and will be called by
  ## 'sonify()' once there are any non-audio rendering options.
  
  if(!(x %in% c("audio")))
    stop("'",x, "' is not a valid rendering")
  if(length(x) > 0 | !is.character(x))
    stop("Renderings must be a character vector of length one.")
  
}

rendering <- function(x) {
  ## Not yet part of public API
  ## This function exists ENTIRELY for changing rendering
  ## in interactive use via '+.sonify';
  ## all it does is check the validitiy of its string argument
  ## and give it the class "sonrendering"
  ## (so that '+.sonify' knows what to do with it)
  
  .checkRendering(x)
  class(x) <- "sonrendering"
  x
}

.checkData <- function(x) {
  ## Checks if sonify object or sonlayer x has an invalid data.frame
  ## It is okay for x$data to be null since one of either the sonify
  ## object or the layers can have NULL data frame.
  ## Called by 'sonify()' and 'sonlayer()'

  if(!is.null(x$data) & !is.data.frame(x$data))
    stop("'data' must be a data.frame.")
}


