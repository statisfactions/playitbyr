\name{sonscaling}
\alias{sonscaling}
\title{Setting scales for \code{sonify} objects}
\description{
  \code{sonscaling()} is invoked in the call to \code{sonify} or added onto
  it in order to specify precisely how the data parameters are mapped
  onto sonic parameters. Currently, only continuous scales are
  supported.
}
\usage{
sonscaling(pitch = NULL, time = NULL, tempo = NULL, dur = NULL, vol =
NULL, pan=NULL, timbre = NULL)
}
\arguments{
    \item{pitch}{The desired pitch mapping of all events, represented
    such that 8 represents middle C, 9 represents the octave above,
    etc. (This is
    \href{http://www.csounds.com/manual/html/cpsoct.html}{Csound's
      \sQuote{oct} notation}. A list with three elements: the minimum
    pitch, maximum pitch, and the scaling function.)}
  \item{time}{The desired start
    time mapping of all events (in seconds). A list with three elements:
  the minimum time, maximum time, and the scaling function.}
  \item{tempo}{The desired tempo mapping (in beats per minute). A list
    with three elements: the minimum tempo, the maximum tempo, and the
    scaling function.}
  \item{dur}{The desired duration mapping (in seconds). A list with
    three elements: the minimum duration, the maximum duration, and the
    scaling function.}
  \item{vol}{The desired volume mapping as a number between 0, silence, and 1, the maximum
  possible amplitude. A list with three elements: the minimum volume,
  the maximum volume, and the scaling function.}
  \item{pan}{The desired mapping for balance
    between left and right stereo channels, where 0 is all the left
    channel, 0.5 balanced, and 1 all the right. A list with three
    elements: the minimum pan, the maximum pan, and the scaling
    function.}
}

\seealso{
  \code{\link{sonify}} where this is eventually used;
  \code{\link{sonaes}} for defining the which data columns get mapped
  onto these sonic parameters; \code{\link{scalingShortcuts}} for easy
  shortcut functions for common use-cases of scaling; and
  \code{\link{linear.scale}} for an example of a scaling function.}
