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
  stop("need to fix render.csound to harmonize with createPerformance")
  orcpath <- opts$orcpath
  flags <- opts$flags

  ## Create i statement list of matrices
  i <- lapply(x, as.matrix)

  f <- attributes(x)$render_options$f

  createPerformance(i = i, f=f, orcpath)
    
}


