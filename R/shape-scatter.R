##' Create a scatterplot layer
##'
##' Creates a scatterplot layer; the audio analogue of \pkg{ggplot2}'s
##' \code{geom_point}.
##'
##' The audio scatterplot is implemented by a simple
##' frequency-modulation synthesis (through csound). The following
##' parameters are available for setting or mapping:
##'
##' \describe{
##' \item{time}{The starting time of the note (in seconds).}
##' \item{pitch}{The pitch of the note, in the Csound
##' \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
##' notation} notation for pitches, where 8 is middle C and 1
##' represents an octave, to the corresponding frequency in Hertz.
##' By default this is scaled to the nearest musical (chromatic) pitch.}
##' \item{dur}{The duration of the note (relative to the total time if \code{relative = TRUE}, in seconds otherwise).}
##' \item{amp}{The volume of the note, as a proportion between 0 and
##' 1, where 1 is the maximum volume. Note that a multiple notes that happen
##' at the same time could add up to more than one, causing distortion an
##' clipping.}
##' \item{attkp}{The proportion of the note's length devoted to the initial (linear)
##' attack.}
##' \item{decayp}{The proportion of the note's length devoted to the (linear) decay.}
##' \item{mod}{The modulating frequency, given as a \emph{multiple}
##' of the carrier tone.}
##' \item{indx}{The index of modulation.}
##' }
##'
##' To \emph{set} a sound parameter to a value, you simply include it
##' as an extra argument in \code{shape_scatter}; to \emph{map} a
##' parameter, you set the mapping for the layer or the \code{sonify}
##' object using \code{\link{sonaes}} (see examples).
##'
##' @param jitter The maximum size, in seconds, of how much to jitter
##' time by when there are multiple notes at the same pitch and time
##' (the sonic equivalent of overplotting). The default, 0, means no
##' jitter occurs.
##' @param relative Make the duration relative to the overall length
##' of the sonification? The default, \code{TRUE}, means that the
##' sonification will scale durations relative to the length of the
##' overall sonification (it estimates how long a \dQuote{beat} is and
##' then rescales duration in relation to that). Otherwise, durations
##' remain constant even if the sonification is much longer or shorter
##' (which may mean that note durations must be fiddled with so they
##' don't overlap). 
##' @param \dots data, settings, and mappings to pass to
##' \code{\link{sonlayer}}
##' 
##' @return A \code{sonlayer} object
##' 
##' @export
shape_scatter <- function(jitter = 0, relative = TRUE, ...) sonlayer("scatter", jitter = jitter, relative = relative, ...)

