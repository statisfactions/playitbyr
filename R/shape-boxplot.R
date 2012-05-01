##' Create a boxplot layer
##'
##' Gives a sense of the distribution of values by sampling at
##' increasingly narrow percentiles around the median.
##'
##' The audio boxplot is implemented by a simple frequency-modulation
##' synthesis (through csound), representing a univariate view of the
##' data by rapidly playing pitches in the 5th to 95th percentile,
##' then the interquartile range (25th to 75th), then the median, all
##' sampled from the dataset. (It is essentially
##' \code{\link{shape_histogram}} evaluated separately on each of
##' these subsets of the data.)
##'
##' Only \code{pitch} is intended
##' to be used for mapping but the remainder of the same parameters
##' available in \code{shape_scatter} are available for setting to
##' suit the analyst's ear:
##'
##' \describe{
##' \item{time}{The starting time of the note (in seconds).}
##' \item{pitch}{The pitch of the note, in the Csound
##' \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
##' notation} notation for pitches, where 8 is middle C and 1
##' represents an octave, to the corresponding frequency in Hertz.  By
##' default this is scaled to the nearest musical (chromatic)
##' pitch. (See \code{\link{scale_pitch_continuous}}.)}
##' \item{dur}{The duration of the note (relative to the total time if \code{relative = TRUE}, in seconds otherwise).}
##' \item{amp}{The volume of the note, as a proportion between 0 and
##' 1, where 1 is the maximum volume. Note that a multiple notes that happen
##' at the same time could add up to more than one, causing distortion and
##' clipping.}
##' \item{attkp}{The proportion of the note's length devoted to the initial (linear)
##' attack.}
##' \item{decayp}{The proportion of the note's length devoted to the (linear) decay.}
##' \item{indx}{The index of modulation. This affects the distortion of the tone; \code{indx = 0} is a sine wave, whereas higher indices of modulation give increasingly complex tones.}
##' \item{mod}{The modulating frequency, given as a multiple
##' of the primary frequency (i.e. given by \code{pitch}).}
##' }
##'
##' To \emph{set} a sound parameter to a value, you simply include it
##' as an extra argument in \code{shape_scatter}; to \emph{map} a
##' parameter, you set the mapping for the layer or the \code{sonify}
##' object using \code{\link{sonaes}} (see examples in \code{\link{shape_scatter}}).
##'
##' @param length The overall length, in seconds, of each segment of the boxplot.
##' @param tempo The tempo, in beats per minute; i.e. the rapidity
##' with which to represent values from the dataset.
##' @param pause The pause between each segment of the boxplot, in seconds.
##' @inheritParams sonlayer
##' 
##' @param \dots settings to pass to
##' \code{\link{sonlayer}} (see Details)
##' 
##' @return A \code{sonlayer} object that can be added onto a \code{\link{sonify}} object.
##' @references S. Ferguson, W. Martens and D. Cabrera, ``Statistical Sonification for Exploratory Data Analysis'', in \emph{The Sonification Handbook},  ed. Hermann, Hunt, Neuhoff. Available: \url{http://sonification.de/handbook/}
##' @examples
##' x1 <- sonify(iris, sonaes(pitch = Sepal.Length)) + sonfacet(Species) +
##'    shape_boxplot(length = 1, tempo = 1800)
##' \dontrun{x1} # facet by Species
##' x2 <- sonify(iris, sonaes(pitch = Sepal.Length)) +
##'   shape_boxplot(length = 2, tempo = 1800) # plays each segment longer
##' \dontrun{x2}
##' x3 <- sonify(iris, sonaes(pitch = Sepal.Length)) +
##'   shape_boxplot(length = 1, tempo = 1200) #  same length as original but fewer pitches
##' \dontrun{x3}
##' @export
shape_boxplot <- function(length = 5, tempo = 240, pause = 0.1, ..., data = NULL,
                          mapping = NULL) sonlayer("boxplot", length = length, tempo = tempo, pause = pause, data = data, mapping = mapping, ...)

