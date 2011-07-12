sonify <- function(data=NULL, mapping=sonaes(), scales=sonscaling()) {
  ## This just puts the items in a list
  ## TODO much more validation is needed to make this sensible

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
  class(s) <- c(rendering, "sonify", "list")
  s
}

##Need to generate mappings. Let's tempo with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NULL, time=NULL, tempo=NULL, dur=NULL, vol=NULL, pan=0.5, timbre="sine") {
  ##Similar to ggplot2 "sonaes"
  
  son <- list(pitch, time, tempo, dur, vol, pan, timbre) #TODO: make this easier to add on to
  names(son) <- c("pitch", "time", "tempo", "dur", "vol", "pan", "timbre") #TODO: extract this  intelligently
  class(son) <- c("sonaes", "character")
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
      if(!is.null(y[[i]])) x$scales[[i]] <- y[[i]]
    }
  } else if("sonaes" %in% class(y)) {
    for(i in names(x$mapping)) {
      if(!is.null(y[[i]])) x$mapping[[i]] <- y[[i]]
    }
  } else if("sonrendering" %in% class(y)) {
    x$rendering <- y
    class(x) <- c(y, class(x)[-1])
  } else {stop("'+' operator not supported for this operation.")}
  x
}          

render <- function(s) UseMethod("render")

print.sonify <- function(x, ...) {
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
