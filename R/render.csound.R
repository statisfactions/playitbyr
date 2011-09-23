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
  
  orcpath <- attributes(x)$render_options$orcpath
  flags <- attributes(x)$render_options$flags

  if(is.null(flags))
    flags <- c("-odac", "-g")
  
  
  ## Create i statement list of matrices
  i <- lapply(x, as.matrix)

  f <- attributes(x)$render_options$f

  createPerformance(orcpath, i = i, f=f)
    
}


