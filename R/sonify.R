sonify <- function(data=NULL, mapping=sonaes(), scales=sonscaling()) {
  ## This just puts the items in a list
  ## TODO much more validation is needed to make this sensible
  if(!is.null(data) & !is.data.frame(data))
    stop("'data' must be a data.frame.")
  
  ## TODO these functions will eventually be defined separately
  sonlayer <- function(shape="notes", shape_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL) {
    l <- list(list(shape, shape_params), list(stat, stat_params), data, mapping)
    names(l) <- c("shape", "stat", "data", "mapping")
    names(l$stat) <- c("stat", "stat_params")
    names(l$shape) <- c("shape", "shape_params")
    class(l) <- c("sonlayer", "list")
    l
  }
  shape_notes <- function(...) sonlayer("notes",...)

  rendering <- function(x) {
    class(x) <- "sonrendering"
    x
  }

  
  ## TODO these will eventually be arguments in the sonify function,
  ## but for now I'm simply assigning them since there is only one
  ## possible value
  sonlayers <- shape_notes()
  rendering <- "audio"
  dataname <- deparse(substitute(data))
  s <- list(data, dataname, mapping, rendering, scales, sonlayers)
  names(s) <- c("data", "dataname", "mapping", "rendering", "scales", "sonlayers") 
  class(s) <- c(rendering, "sonify")
  s
}

##Need to generate mappings. Let's tempo with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(time=0, pitch=8, dur=2, vol=1, pan=0.5, tempo=NULL, timbre="sine") {
  ##Similar to ggplot2 "sonaes"

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
  xname <- deparse(substitute(x))
  map <- .getMappings(x, 1)
  ## Checks that correct mapping slots are filled
  if(is.null(x$data))
    stop("No data.frame provided for sonification. See ?sonify.")
  
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

render <- function(s) UseMethod("render")

print.sonify <- function(x, ...) {
  checkSonify(x)
  render(x)
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
  ##If mappings are already set up, the input data.frame should have the
  ##same names, which we explicitly should check with sonthing like:
  ##  if(!all(x$mapping[!is.null(x$mapping)] %in% names(y)))
  ##    stop("New data.frame does not match mapping in sonify object")
  ## THe preceding is not quite correct since it doesn't deal with the fact that some
  ## mappings are set
  x$data <- y
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
