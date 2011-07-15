\name{render}
\alias{render}
\alias{playAudioRendering}
\usage{
render(x)
playAudioRendering(audioSamp)
}
\value{
  
  \code{render.audio} returns an \code{audioSample} object (from the
  \code{audio} package) of the sonification.

  \code{playAudioRendering} is called for its side effect, to produce
  the sound. It is just a wrapper for \code{\link[audio]{play}} on
  Windows and OS X; on Linux systems it creates a temporary file and
  plays it with an external \code{*.wav} file player (see
  \code{\link{setPlayer}}). 

}

\title{Render \code{sonify} objects to \code{audioSample}}

\arguments{
  \item{x}{A \code{sonify} object to be rendered.}
  \item{audioSamp}{An \code{audioSample} object, already rendered and
    ready to be played or saved to file.}
}

\seealso{\code{\link[audio]{save.wave}} for saving \code{audioSample}
  objects to file; \code{\link{setPlayer}} for getting and setting the
  method of actually playing the rendering; and \code{\link{sonify}} for
  general details.}

\examples{
x <- sonify(iris) + sonaes(Petal.Length, Petal.Width)
out <- render(x)
\dontrun{
playAudioRendering(out)
## This is the same as just printing the object:
x

## But you can also save it to file using the audio package:
save.wave(out, "myfile.wav")
}
}
\details{
\code{\link{print.sonify}} calls both these functions; these are
provided in case you want to manipulate the object further or save it to file.
}

