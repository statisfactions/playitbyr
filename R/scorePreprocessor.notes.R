##' Score preprocessing for shape 'notes'
##'
##' This function does some needed preprocessing for shape notes to
##' get the method ready to render. In this case, it takes the 'time'
##' and 'tempo' mappings and translates them into start times. It also
##' scales duration accordingly.
##' 
##' @keywords internal
##' @method scorePreprocessor notes
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the 'Description' field
scorePreprocessor.notes <- function(sonlayerscore) {
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

  ## Scale durations by total time divided by number of notes
  sonlayerscore$dur <- (sonlayerscore$dur) * (total/n)

  ## (NOTE: this is somewhat questionable whether this is the right
  ## thing to do; it's a little arbitrary and makes scaling duration
  ## less intuitively related to what's specified in x$scaling.  I
  ## have chosen to do it this way b/c it means you can easily set a
  ## different scaling for the time and duration will automagically
  ## scale down without having to set it separately, which seems
  ## annoying.)
  
  attr(sonlayerscore, "length") <- sonlayerscore$start[n] + sonlayerscore$dur[n] # length in seconds

  return(sonlayerscore)
}
