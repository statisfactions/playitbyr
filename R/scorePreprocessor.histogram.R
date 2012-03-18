##' Score preprocessing for shape 'histogram'
##'
##' This function does some needed preprocessing for shape histogram to
##' get the method ready to render. In this case, it takes the 'time'
##' and 'tempo' mappings and translates them into start times. It also
##' scales duration accordingly.
##' 
##' @keywords internal
##' @method scorePreprocessor histogram
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the 'Description' field
scorePreprocessor.histogram <- function(sonlayerscore, opts, ...) {
  ## We need to transform the "tempo" or "time" data into actual start
  ## times and tranform the durations accordingly

  n <- ceiling(opts$tempo*opts$length/60)
  rows <- sample(1:nrow(sonlayerscore), n, replace = TRUE)
  out <- sonlayerscore[rows,]
  dur <- opts$length/n
  out$time <- NULL
  out$start <- seq(0, opts$length, by = dur)[-n]
  out$dur <- dur

  attr(out, "length") <- opts$length

  return(out)
}
