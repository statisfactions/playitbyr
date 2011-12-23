##' Rendering sound using csound
##'
##' \code{render.csound} uses the csound package to render a
##' \code{score} object.
##'
##' @rdname render.csound
##' @inheritParams render
##' @param \dots Currently ignored
##' @note See \code{\link{createPerformance}} for possible
##' \code{render_options} that can be used with this rendering
##' technique; all the arguments of \code{createPerformance} can be
##' passed as options.
render.csound <- function(x, opts, audioSample=FALSE, ...) {
  ## Draw options only from first sonlayer. Annoying, I know. Really,
  ## these are rendering, not shape options
  i <- lapply(x, function(y) csound_layer(y))
  createPerformance(i)
}

##' @rdname render.csound
##' @param sonlayerscore An element of the score list--the score
##' produced for a specific layer. The class of this determines the
##' shape to be rendered
csound_layer <- function(sonlayerscore, ...) {
  ## Generic to render a shape
   UseMethod("csound_layer")
}

##' @rdname render.csound
##' @method csound_layer scatter
csound_layer.scatter <- function(sonlayerscore, ...) {
  sonlayerscorem <- as.matrix(sonlayerscore)
  out <- scoreMatrices(nrow(sonlayerscorem))
  out$FM[, c("dur", "pan", "start", "amp")] <- sonlayerscorem[, c("dur", "pan", "start", "vol")]
  out$FM[, "cps"] <- octToFreq(sonlayerscorem[, "pitch"])
  out$FM[, c("attkp", "decayp")] <- 0.01
  out$FM[, "mod"] <- 1
  out$FM[, "indx"] <- 1
  return(out$FM)
}


