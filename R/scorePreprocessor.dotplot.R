##' Score preprocessing for shape 'dotplot'
##'
##' This function does some needed preprocessing for shape dotplot to
##' get the method ready to render. In this case, it takes the 'time'
##' and 'tempo' mappings and translates them into start times. It also
##' scales duration accordingly.
##' 
##' @keywords internal
##' @method scorePreprocessor dotplot
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the 'Description' field
scorePreprocessor.dotplot <- function(sonlayerscore, opts, ...) {
  ## We need to transform the "tempo" or "time" data into actual start
  ## times and tranform the durations accordingly

  n <- nrow(sonlayerscore)

  if("tempo" %in% names(sonlayerscore)) {
    ## If tempo is provided, convert tempo data into start times, sort
    ## score, and scale durations in relation to beat
    beatlength <- 60/sonlayerscore$tempo
    sonlayerscore$tempo <- NULL
    sonlayerscore$start <- c(0, cumsum(beatlength[-n]))
    total <- sonlayerscore$start[n]+ beatlength[n] #used to calculate dur
  } else if("time" %in% names(sonlayerscore)) {
    ## Otherwise, just rename 'time' to 'start' and sort by that.
    sonlayerscore$start <- sonlayerscore$time
    sonlayerscore$time <- NULL
    sonlayerscore <- sonlayerscore[order(sonlayerscore$start),]
    total <- sonlayerscore$start[n] + mean(sonlayerscore$start[-1] - sonlayerscore$start[-n]) #used to calculate dur
  }

  ## add jitter effect
  if(!is.null(opts$jitter))
  sonsplit <- split(sonlayerscore, 10^(-5)*(sonlayerscore$start) + sonlayerscore$pitch)
  sonlayerscore <- do.call(rbind, lapply(sonsplit, function(x) {
    if(nrow(x) > 1)
      x$start <- abs(x$start + rnorm(nrow(x), mean = 0, sd = opts$jitter))
    return(x)
  }))
  
  attr(sonlayerscore, "length") <- max(sonlayerscore$start + sonlayerscore$dur) # length in seconds

  return(sonlayerscore)
}
