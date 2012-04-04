##' Score preprocessing for shape 'scatter'
##'
##' This function does some needed preprocessing for shape scatter to
##' get the method ready to render. In this case, it takes the 'time'
##' and 'tempo' mappings and translates them into start times. It also
##' scales duration accordingly.
##' 
##' @keywords internal
##' @method scorePreprocessor scatter
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the 'Description' field
scorePreprocessor.curvepair <- function(sonlayerscore, opts, ...) {
  n <- nrow(sonlayerscore)

  ## this definitely assumes a kind of uniqueness, the entire thing does
  firstrow <- sonlayerscore[1, setdiff(.getSoundParams("curvepair")$param, c("ref", "compare", "time", "pitch", "dur", "tempo"))]

  scorereally <- lapply(1:n, function(i) {
    signage <- sign(sonlayerscore$compare[i] - sonlayerscore$ref[i])
    betweens <- seq(sonlayerscore$ref[i], sonlayerscore$compare[i], by = signage*(1/12))
    starts <- exp_scale(1:(length(betweens)+1),
                       soundlimits = c(0, opts$length))
    durs <- diff(starts)
    starts <- starts[-length(starts)]
    out <- data.frame(start = starts, dur = durs, pitch = betweens)
    attr(out, "length") <- opts$length
    return(out)
  })

  joined <- facetjoin(scorereally, 0)
  joined[names(firstrow)] <- firstrow
  return(joined)
}
