##' Create mappings and parameters for sonification objects
##' 
##' Creates a \code{sonaes} object, which is a list containing the desired
##' mappings of data columns or constants onto each sound parameter. This is
##' intended be added to a \code{sonify} object (or included in its
##' construction).
##' 
##' Setting sonic parameters to \code{data.frame} columns or constant values is
##' the heart of making the sonification happen and making it possible to
##' render.
##' 
##' All fields, except for one of \code{time} or \code{tempo}, must be assinged
##' in the \code{sonify} before rendering to sound. \code{sonaes} does not
##' check to make sure that all mappings are filled in. since the user can add
##' on more mappings (using \code{\link{+.sonify}}) interectively. However,
##' this is checked by \code{\link{checkSonify}} when the object is to be
##' rendered.
##' 
##' Also, every item that is mapped to a \code{data.frame} column in a
##' \code{sonify} object must also have a scale associated with it before
##' rendering; see \code{\link{sonscaling}}.
##' 
##' @param time a \code{data.frame} column name corresponding to one of the
##' columns of the \code{data} argument of \code{\link{sonify}}, or the desired
##' start time of all events (in seconds). Either \code{time} or \code{tempo}
##' must be specified, but not both, for a \code{sonify} object to be rendered.
##' For any of these arguments, quotes are optional.
##' @param pitch a \code{data.frame} column name or the desired pitch of all
##' events, represented such that 8 represents middle C, 9 represents the
##' octave above, etc. (This is
##' \href{http://www.csounds.com/manual/html/cpsoct.html}{Csound's \sQuote{oct}
##' notation}.)
##' @param dur a \code{data.frame} column name, or the relative desired
##' duration of all events in \dfn{beats}, where 1 beat equals the length of
##' time for one event if all events were equal length.
##' @param vol a \code{data.frame} column name, or the desired volume of all
##' events as a number between 0, silence, and 1, the maximum possible
##' amplitude
##' @param pan a \code{data.frame} column name, or the desired balance between
##' left and right stereo channels, where 0 is all the left channel, 0.5
##' balanced, and 1 all the right.
##' @param tempo a \code{data.frame} column name, or the desired tempo of all
##' events (in beats per minute)
##' @param timbre The desired timbre of the sound. Currently only the default
##' value "sine", for a simple sine wave, is supported.
##' @return A \code{sonaes} object, used in or added to a \code{sonify} object.
##' @seealso
##' \code{\link{sonify}}, \code{\link{sonscaling}}, \code{\link{octToFreq} }.
##' Also, see \code{\link[ggplot2]{aes}} from the \pkg{ggplot2} package, which
##' inspired this function.
##' @examples
##' 
##' ## Maps Petal.Width onto tempo,
##' ## Sepal.Width onto pitch,
##' ## and sets scalings
##' ## (not terribly informative, since the order is
##' ## arbitrary)
##' x <- sonify(iris, sonaes(tempo=Petal.Width,
##'             pitch=Sepal.Width))
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
sonaes <- function(time=0, pitch=8, dur=2, vol=1, pan=0.5, tempo=NULL, timbre="sine") {

  ## 'sonaes' objects are lists and are used as the top-level
  ## 'mapping' slot of sonify objects

##################################################
### BRAINSTORM 2011-08-30
###
### Here's where I'm defining the data structure, used BOTH by the
### default mapping slot and by each layer. This data structure deeply
### accessed in .get, but it's also
### accessed by .checkSonify, summary.sonify, and +.sonify, confirmed
### by a quick grep of the R files.
  
  if(!missing(time) && !missing(tempo))
    stop("Only one of 'time' or 'tempo' can be provided.")

  given <-  as.list(match.call()[-1])
  ## Deparse any unquoted data.frame columns given as args
  given <- lapply(given, function(x) {
                  x <- ifelse(is.symbol(x), deparse(x), x)
                  if(!is.null(x)) attr(x, "default") <- FALSE
                  return(x)})
  son <- lapply(formals(), function(x){
                if(!is.null(x)) attr(x, "default") <- TRUE
                return(x)})
  son[match(names(given), names(son))] <- given

  ## Auto-adjust for one of time, tempo being not given
  if(missing(time) && !missing(tempo))
    son["time"] <- list(NULL)
  if(!missing(time) && missing(tempo))
    son["tempo"] <- list(NULL)

  ## Check validity
  if(!is.null(son$time) && son$time < 0)
    stop("time must be greater than 0.")
  if(!is.null(son$tempo) && (son$tempo<=0))
    stop("tempo must be greater than 0 (bpm)")
  if(son$dur<0)
    stop("dur cannot be negative")
  if(((son$vol<0) || (son$vol>1)))
    stop("vol must be between 0 and 1.")
  if((son$pan<0) || (son$pan>1))
    stop("pan must be between 0 and 1.")
  if(son$timbre != "sine")
    stop("'sine' is the only supported timbre right now")  

  class(son) <- c("sonaes")
  son
}
