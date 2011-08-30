\name{sonscaling}
\alias{sonscaling}
\title{Setting scales for \code{sonify} objects}
\usage{
  sonscaling(time = list(0, 5, linear.scale), pitch =
  list(8, 9, linear.scale), dur = list(0.25, 4,
  linear.scale), vol = list(0.2, 1, linear.scale), pan =
  list(0, 1, linear.scale), tempo = list(120, 240,
  linear.scale), timbre = NULL)
}
\arguments{
  \item{time}{The desired start time mapping of all events
  (in seconds). A list with three elements: the minimum
  time, maximum time, and the scaling function.}

  \item{pitch}{The desired pitch mapping of all events,
  represented such that 8 represents middle C, 9 represents
  the octave above, etc. (This is
  \href{http://www.csounds.com/manual/html/cpsoct.html}{Csound's
  \sQuote{oct} notation}. A list with three elements: the
  minimum pitch, maximum pitch, and the scaling function.)}

  \item{dur}{The desired duration mapping (in seconds). A
  list with three elements: the minimum duration, the
  maximum duration, and the scaling function.}

  \item{vol}{The desired volume mapping as a number between
  0, silence, and 1, the maximum possible amplitude. A list
  with three elements: the minimum volume, the maximum
  volume, and the scaling function.}

  \item{pan}{The desired mapping for balance between left
  and right stereo channels, where 0 is all the left
  channel, 0.5 balanced, and 1 all the right. A list with
  three elements: the minimum pan, the maximum pan, and the
  scaling function.}

  \item{tempo}{The desired tempo mapping (in beats per
  minute). A list with three elements: the minimum tempo,
  the maximum tempo, and the scaling function.}

  \item{timbre}{Ignored. Currently no mappings are
  supported for this timbre}
}
\description{
  \code{sonscaling()} is invoked in the call to
  \code{sonify} or added onto it in order to specify
  precisely how the data parameters are mapped onto sonic
  parameters. Currently, only continuous scales are
  supported.
}
\details{
  Each argument of \code{sonscaling} is in the form of a
  list, where the first element is the minimum value of the
  sonic parameter, the second is the maximum, and the third
  is a function that maps the data column onto the range of
  the sonic parameter. The only such function included with
  \pkg{playitbyr} right now is \code{\link{linear.scale}}
  but users can write their own function for this purpose.
  User-defined scaling functions need to be of the same
  form as \code{\link{linear.scale}}, i.e. it must have
  arguments for the input vector \code{x}, and the
  \code{min} and \code{max}.
}
\seealso{
  \code{\link{sonify}} where this is eventually used;
  \code{\link{sonaes}} for defining the which data columns
  get mapped onto these sonic parameters;
  \code{\link{scaleShortcuts}} for easy shortcut functions
  for common use-cases of scaling; and
  \code{\link{linear.scale}} for an example of a scaling
  function.
}

