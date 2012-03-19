##' Score preprocessing for shape 'boxplot'
##'
##' This function does some needed preprocessing for shape boxplot to
##' get the method ready to render. In this case, it takes the 'time'
##' and 'tempo' mappings and translates them into start times. It also
##' scales duration accordingly.
##' 
##' @keywords internal
##' @method scorePreprocessor boxplot
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the
##' 'Description' field
scorePreprocessor.boxplot <- function(sonlayerscore, opts, ...) {
  ## We need to transform the "tempo" or "time" data into actual start
  ## times and tranform the durations accordingly
  pitch05 <- quantile(sonlayerscore$pitch, 0.05)
  pitch25 <- quantile(sonlayerscore$pitch, 0.25)
  pitch50 <- quantile(sonlayerscore$pitch, 0.5)
  pitch75 <- quantile(sonlayerscore$pitch, 0.75)
  pitch95 <- quantile(sonlayerscore$pitch, 0.95)
  med <- sonlayerscore[1, ]
  med$pitch <- pitch50
  scoresplit <- list(sonlayerscore[sonlayerscore$pitch >= pitch05 & sonlayerscore$pitch <= pitch95,],
                     sonlayerscore[sonlayerscore$pitch >= pitch25 & sonlayerscore$pitch <= pitch75,],
                     med)
  scoredone <- lapply(scoresplit, function(x)
                      scorePreprocessor.histogram(x, opts))
  out <- facetjoin(scoredone, 0.5)
## TODO pause as shape option                   
  return(out)
}
