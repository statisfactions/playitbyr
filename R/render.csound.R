##' Rendering sound using csound
##'
##' \code{render.csound} uses the csound package to render a
##' \code{score} object.
##'
##' @rdname render.csound
##' @param x A \code{score} object created by \code{\link{.getScore}}
##' @param audioSample Currently ignored.
##' @param \dots Currently ignored.
render.csound <- function(x, audioSample=FALSE, ...) {

  ## Draw options only from first sonlayer. Annoying, I know. Really,
  ## these are rendering, not shape options
  
  orcpathname <- x[[1]]$shape_options$orcpath
  flags <- x[[1]]$shape_options$flags

  ## Soon will need something like this to handle 'orctext',
  ## specifying orchestra in R:
  ## 
  ## if(is.null(orcpathnames)) {
  ##   orctext <- unique(sapply(x, function(y) attributes(y)$shape_options$orctext))

  
  ## layers into a single score
  maxcol <- max(sapply(x, function(y) ncol(y)))
  
  
}


