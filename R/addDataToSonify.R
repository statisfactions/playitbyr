##' Add a data.frame to a \code{sonify} object
##'
##' This allows you to add a \code{data.frame} onto a \code{sonify}
##' object with a syntax analogous to \pkg{ggplot2}
##'
##' It would be easier if this operation could be done with
##' \sQuote{+}, but \code{R} does not allow it.
##'
##' @note \code{\%+\%} conflicts with the \code{\%+\%} defined in the
##' \pkg{ggplot2} package. Thus, the identical addDataToSonify is
##' provided as a workaround if you have both packages loaded.
##'
##' @rdname addDataToSonify
##' @name addDataToSonify
##' @aliases %+% addDataToSonify
##' @param x A \code{sonify} object whose default \code{data.frame} is
##' to be changed
##' @param y A \code{data.frame} to add to \code{x}
##' @return A \code{sonify} object with \code{y} now as the default
##' \code{data.frame}
##'
##' @seealso \code{+.sonify}
##' @export
##' @usage x \%+\% y
`%+%` <- function(x, y) {
  ## This function does not check whether the y's names
  ## match the names in x$mapping, but this is checked
  ## before rendering by checkSonify
  
  x$data <- y
  .checkData(x)
  x$dataname <- deparse(substitute(y))
  x
}

##' @rdname addDataToSonify
##' @export
## Alternate syntax for the same thing
addDataToSonify <- function(x, y) x %+% y
