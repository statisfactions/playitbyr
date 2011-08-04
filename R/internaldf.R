

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
