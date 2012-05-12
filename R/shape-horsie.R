##' Create a horsie layer
##'
##' Creates a dotplot layer using the sound of a horse instead of the
##' typical snare drum
##'
##' @param jitter The maximum size, in seconds, of how much to jitter
##' time by when there are multiple notes at the same pitch and time
##' (the sonic equivalent of overplotting). The default, 0, means no
##' jitter occurs.
##' 
##' @inheritParams sonlayer
##' 
##' @param \dots data, settings, and mappings to pass to
##' \code{\link{sonlayer}} (see Details)
##' @return A \code{sonlayer} object that can be added onto a \code{\link{sonify}} object.
##' @seealso \code{\link{shape_dotplot}}
##' @author Sound from Freesound user 3bagbrew, licensed CC-Attribution. \url{http://www.freesound.org/people/3bagbrew/sounds/59569/}
##' @examples
##' x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_horsie(jitter = 0.3)
##' \dontrun{print(x)}
##' @export
shape_horsie <- function(jitter = 0, ..., data = NULL,
                          mapping = NULL) sonlayer("horsie", jitter = jitter, data = data, mapping = mapping, ...)

