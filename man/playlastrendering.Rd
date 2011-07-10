\name{playLastRendering}
\title{Play and save the last audio rendering of a \code{sonify} object}
\alias{.LastRendering}
\alias{playLastRendering}
\alias{saveLastRendering}
\description{
  Whenever a \code{sonify} object is rendered, \code{playitbyr} saves an
  \code{audioSample} object, \code{.LastRendering}, with the audio
  rendering in the user's workspace for later playback or
  manipulation. \code{playLastRendering} and \code{saveLastRendering}
  are convenience functions to play or save this object, but it can also
  be manipulated via the functions in the \code{audio} package.
}
\usage{
playLastRendering()
saveLastRendering(filename)
}
\arguments{
  \item{filename}{A \code{.wav} file to save the \code{audioSample}
    object to.}
}
\value{
  These functions are invoked for their side-effects, which are to play
  or save the \code{.LastRendering} \code{audioSample} object.}
\details{
  These functions are simply convenience wrappers for
  \code{\link[audio]{play}} and \code{\link[audio]{save.wave}} in the
  \code{audio} package.

  The behavior of \code{playLastRendering()} depends on the option
  "audioRendering". If the option is "tempfile", \code{playLastRendering()} looks for an
  audio player set with \code{setPlayer}, renders the \code{audioSample}
  to a temporary file, and plays it with the given player. If the option
  is "audio::play", \code{playLastRendering()} uses the \code{play}
  function from the \code{audio} package (which does not work well on
  Linux systems).
}

\seealso{
  \code{\link[audio]{play}}, \code{\link[audio]{save.wave}},
  \code{\link{audioSample}}, \code{\link{setPlayer}}
  }





    


