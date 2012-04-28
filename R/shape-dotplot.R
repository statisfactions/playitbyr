##' Create a dotplot layer
##'
##' Creates a dotplot layer using the sound of a snare drum.
##'
##' The audio dotplot creates a sense of how the distribution is
##' spread out in time. Users are only expected to really use the
##' \code{time} parameter for mapping, but they may want to fiddle with the
##' other aspects of the sound for aesthetic reasons.
##'
##' When more than one value appears at a given time, this shape
##' automatically adds noise to "jitter" the values so they can be
##' heard distinctly. 
##'
##' These parameters are available for setting or mapping:
##'
##' \describe{
##' \item{time}{The starting time of the snare sound (in seconds).}
##' \item{pitch}{The pitch of the note, in the Csound
##' \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
##' notation} notation for pitches, where 8 is middle C and 1
##' represents an octave, to the corresponding frequency in Hertz.  By
##' default this is scaled to the nearest musical (chromatic)
##' pitch. (See \code{\link{scale_pitch_continuous}}.)}
##' \item{dur}{The duration of the note (relative to the total time if \code{relative = TRUE}, in seconds otherwise).}
##' }
##'
##' To \emph{set} a sound parameter to a value, you simply include it
##' as an extra argument in \code{shape_scatter}; to \emph{map} a
##' parameter, you set the mapping for the layer or the \code{sonify}
##' object using \code{\link{sonaes}} (see examples in \code{\link{shape_scatter}}).
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
##' @author Originally contributed by \href{http://datasearch2.uts.edu.au/feit/staff/listing/details.cfm?StaffId=7920}{Sam Ferguson}. Csound instrument created by \href{http://csounds.com/mikelson}{Hans Mikelson}.
##' @references S. Ferguson, W. Martens and D. Cabrera, ``Statistical Sonification for Exploratory Data Analysis'', in \emph{The Sonification Handbook},  ed. Hermann, Hunt, Neuhoff. Available: \url{http://sonification.de/handbook/}
##' @examples
##' x <- sonify(iris[1:10,], sonaes(time = Petal.Length)) + shape_dotplot(jitter = 0.3)
##' \dontrun{print(x)}
##' @export
shape_dotplot <- function(jitter = 0, ..., data = NULL,
                          mapping = NULL) sonlayer("dotplot", jitter = jitter, data = data, mapping = mapping, ...)

