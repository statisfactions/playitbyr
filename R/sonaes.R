##' Create mappings and parameters for sonification objects
##' 
##' Creates a \code{sonaes} object, which is a list containing the desired
##' mappings of data columns or constants onto each sound parameter. This is
##' intended be added to a \code{sonify} object (or included in its
##' construction).
##' 
##' Setting sonic parameters to \code{data.frame} columns or constant
##' values is the heart of making the sonification happen and making
##' it possible to render. But {sonaes} does not check to make sure that all mappings
##' are filled in, since the user can add on more mappings (using
##' \code{\link{+.sonify}}) interactively. However, this is checked by
##' \code{\link{checkSonify}} when the object is to be rendered.
##' 
##' Also, every item that is mapped to a \code{data.frame} column in a
##' \code{sonify} object must also have a scale associated with it before
##' rendering; see \code{\link{sonscaling}}.
##'
##' @seealso \code{\link{sonify}}, \code{\link{sonscaling}},
##' \code{\link{octToFreq} }.  Also, see \code{\link[ggplot2]{aes}}
##' from the \pkg{ggplot2} package, which inspired this function.
##' @examples
##' 
##' ## Maps Petal.Width onto tempo,
##' ## Sepal.Width onto pitch,
##' ## and sets scalings
##' ## (not terribly informative, since the order is
##' ## arbitrary)
##' x <- sonify(iris, sonaes(tempo=Petal.Width,
##'             pitch=Sepal.Width)) + shape_notes()
##' summary(x)
##' \dontrun{x ## Quite long!}
##' 
##' ## Use a different tempo scaling than default
##' ## to hear more of the data faster
##' \dontrun{x + scale_tempo_linear(300, 1000)}
##' 
##' ## Map Petal.Width onto 'time' instead
##' y <- x + sonaes(time=Petal.Width)
##' summary(y)
##' \dontrun{y}
##' 
##' ## Seems clipped, so increase duration
##' \dontrun{y + sonaes(dur=4)}
##' 
##' @export
##' @param \dots Sonic parameter mappings, specific to the shape being
##' rendered. All arguments must be named, with the argument names
##' sonic parameters and the values either numeric values (to set the
##' sonic parameter to that value as a constant) or a column of the
##' data.frame that is to be sonified (with or without quotes).
##' @return A \code{sonaes} object, used in or added to a
##' \code{sonify} object or \code{sonlayer}.
sonaes <- function(...) {
  ## 'sonaes' objects are lists and are used as the top-level
  ## 'mapping' slot of sonify and sonlayer objects
  ## This mouthful puts args in list safely without evaluating them,
  ## to allow for specifying data.frame names without quotes:
  out <- as.list(substitute(list(...)))[-1L]

  ## Deparse (convert to string) any unquoted string arguments, which R
  ## interpreted as mode "name" in above:
  out <- lapply(out, function(x) {
    if(is.name(x))
      x <- deparse(x)
    return(x)
  })
  
  if(length(out) > 0
     && is.null(names(out)) | any(names(out) == ""))
    stop("All arguments must be named.")

  checkSoundParams(names(out))
  
  class(out) <- c("sonaes")
  out
}
