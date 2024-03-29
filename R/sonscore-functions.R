##' @export
sonscore <- function(son, scofile) {
  x <- .getScore(son)
  
  len <- attr(x, "length")
  i <- lapply(x, function(y) csound_layer(y, len = len))
  i <- unlist(i, recursive = FALSE)

  writeCsoundScore(i, f = NULL, scofile)
}

##' @export
system_sonsave <- function(son, wavfile) {
  orcfile <- system.file("orc/playitbyr.orc", package = "playitbyr")
  scofile <- sonscore(son, tempfile())
  system2("csound", c(orcfile, scofile, paste("-o", wavfile)))
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

##' @rdname render.csound
##' @method csound_layer dotplot
csound_layer.dotplot <- function(sonlayerscore, ...) {
  len <- list(...)[["len"]]
  allnames <- c("inst", "start", "dur", "p4", "pitch", "pan", "p7", "p8", "p9", "p10", "p11", "p12", "p13")

  sonlayerscorem <- as.matrix(sonlayerscore)
  out <- matrix(nrow = nrow(sonlayerscorem), ncol = length(allnames))
  colnames(out) <- allnames
  namesmatch <- intersect(colnames(sonlayerscorem), allnames)

  out[, "inst"] <- instSnare
  out[, namesmatch] <- sonlayerscorem[, namesmatch]
  out[, "p4"] <- 30000
  out[, 7:13] <- t(replicate(nrow(out), c( .5, .2, 1, .5, .5, 1, 1.5)))

  noisegen <-  rbind(c(instSnareSupport, 0, len + 0.2, .5, 1),
                     c(instSnareSupport, 0, 0, 0, 0))

  return(list(noisegen, out))
 }

##' @rdname render.csound
##' @method csound_layer histogram
csound_layer.histogram <- function(sonlayerscore, ...) {
  sonlayerscorem <- as.matrix(sonlayerscore)
  out <- scoreMatrices(nrow(sonlayerscorem))
  namesmatch <- intersect(colnames(sonlayerscorem), colnames(out$FM))
                            
  out$FM[, namesmatch] <- sonlayerscorem[, namesmatch]
  out$FM[, "amp"] <- sonlayerscorem[, "vol"]
  out$FM[, "cps"] <- .octToFreq(sonlayerscorem[, "pitch"])
  return(list(out$FM))
}

##' @rdname render.csound
##' @method csound_layer boxplot
csound_layer.boxplot <- function(sonlayerscore, ...) {
  sonlayerscorem <- as.matrix(sonlayerscore)
  out <- scoreMatrices(nrow(sonlayerscorem))
  namesmatch <- intersect(colnames(sonlayerscorem), colnames(out$FM))
                            
  out$FM[, namesmatch] <- sonlayerscorem[, namesmatch]
  out$FM[, "amp"] <- sonlayerscorem[, "vol"]
  out$FM[, "cps"] <- .octToFreq(sonlayerscorem[, "pitch"])
  return(list(out$FM))
}

##' @rdname render.csound
##' @method csound_layer boxplot
csound_layer.curvepair <- function(sonlayerscore, ...) {
  sonlayerscorem <- as.matrix(sonlayerscore)
  out <- scoreMatrices(nrow(sonlayerscorem))
  namesmatch <- intersect(colnames(sonlayerscorem), colnames(out$FM))
                            
  out$FM[, namesmatch] <- sonlayerscorem[, namesmatch]
  out$FM[, "amp"] <- sonlayerscorem[, "vol"]
  out$FM[, "cps"] <- .octToFreq(sonlayerscorem[, "pitch"])
  return(list(out$FM))
}
