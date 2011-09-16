##' Score preprocessing for shape 'csound'
##'
##' This does preprocessing for shape 'csound'. All this does is
##' return the score with attribute 'length' added.
##' 
##' @keywords internal
##' @method scorePreprocessor csound
##' @inheritParams scorePreprocessor
##' @return A sonlayer score with the transformations described in the 'Description' field
scorePreprocessor.csound <- function(sonlayerscore) {

  attr(sonlayerscore, "length") <- max(rowSums(sonlayerscore[,c("start", "dur")])) # length in seconds

  return(sonlayerscore)
}
