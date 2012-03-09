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
render.csound <- function(x, opts, file = "", audioSample=FALSE, ...) {
  i <- lapply(x, function(y) csound_layer(y))
  i <- unlist(i, recursive = FALSE)
  if(file == "")
    file <- "dac"

 if("i" %in% names(opts))
   opts$i <- c(opts$i, i)
 else
    opts$i <- i
  
  opts$out <- file
  opts$rendering <- NULL
  do.call(createPerformance, opts)
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
  namesmatch <- intersect(colnames(sonlayerscorem), colnames(out$FM))
                            
  out$FM[, namesmatch] <- sonlayerscorem[, namesmatch]
  out$FM[, "amp"] <- sonlayerscorem[, "vol"]
  out$FM[, "cps"] <- .octToFreq(sonlayerscorem[, "pitch"])
  return(list(out$FM))
}

##' @rdname render.csound
##' @method csound_layer csound
csound_layer.csound <- function(sonlayerscore, ...) {
  list(as.matrix(sonlayerscore))
}

## ; Pink Noise
## ;	Sta	Dur	Seed	Out
## i
## i

## ;	Sta	Dur	Amp	Fqc	Pan	Q	SprDec	SprTone	SprMix	SprQ	PBend	PBTime
## i34	0.5	.5	30000	8.00	.5	.2	1	.5	.5	1	1.5	.1


