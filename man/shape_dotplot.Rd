\name{shape_dotplot}
\alias{shape_dotplot}
\title{Create a dotplot layer}
\usage{
  shape_dotplot(jitter = 0, ...)
}
\arguments{
  \item{jitter}{The maximum size, in seconds, of how much
  to jitter time by when there are multiple notes at the
  same pitch and time (the sonic equivalent of
  overplotting). The default, 0, means no jitter occurs.}

  \item{\dots}{data, settings, and mappings to pass to
  \code{\link{sonlayer}}}
}
\value{
  A \code{sonscaling} object
}
\description{
  Creates a dotplot layer using the sound of a snare drum.
}
\details{
  The audio dotplot creates a sense of how the distribution
  is spread out in time. Users are only expected to really
  use the \code{time} parameter for mapping, but they may
  want to fiddle with the other aspects of the sound for
  aesthetic reasons.

  When more than one value appears at a given time, this
  shape automatically adds noise to "jitter" the values so
  they can be heard distinctly.

  These parameters are available for setting or mapping:

  \describe{ \item{time}{The starting time of the snare
  sound (in seconds).} \item{dur}{The duration of the drum
  sound (in seconds).} \item{pitch}{The pitch of the drum
  sound, in the Csound
  \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
  notation} notation for pitches, where 8 is middle C and 1
  represents an octave, to the corresponding frequency in
  Hertz.} }

  To \emph{set} a sound parameter to a value, you simply
  include it as an extra argument in \code{shape_scatter};
  to \emph{map} a parameter, you set the mapping for the
  layer or the \code{sonify} object using
  \code{\link{sonaes}} (see examples).
}

