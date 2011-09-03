\name{create_audioSample}
\alias{create_audioSample}
\alias{play_audioSample}
\title{Render \code{sonify} objects to \code{audioSample}}
\usage{
  create_audioSample(x, play = FALSE)

  play_audioSample(audioSamp)
}
\arguments{
  \item{x}{A \code{sonify} object to be rendered.}

  \item{...}{Other arguments to be passed to the individual
  rendering option.}

  \item{play}{Logical indicating whether the created
  audioSample should be played immediately}

  \item{audioSamp}{An \code{audioSample} object, already
  rendered and ready to be played or saved to file.}
}
\value{
  \code{render.audio} returns an \code{audioSample} object
  (from the \code{audio} package) of the sonification.

  \code{play_audioSample} is called for its side effect, to
  produce the sound. It is just a wrapper for
  \code{\link[audio]{play}} on Windows and OS X; on Linux
  systems it creates a temporary file and plays it with an
  external \code{*.wav} file player (see
  \code{\link{setPlayer}}).
}
\description{
  These functions provide the ability to render
  sonifications to a \code{audioSample} object from the
  \pkg{audio} package, and save to a file.
  \code{\link{print.sonify}} calls both these functions in
  succession.
}
\examples{
x <- sonify(iris) + sonaes(time = Petal.Length, pitch = Petal.Width)
out <- create_audioSample(x)
\dontrun{
play_audioSample(out)
## This is the same as just printing the object:
x

## But you can also save it to file using the audio package:
save.wave(out, "myfile.wav")
}
}
\seealso{
  \code{\link[audio]{save.wave}} for saving
  \code{audioSample} objects to file;
  \code{\link{setPlayer}} for getting and setting the
  method of actually playing the rendering; and
  \code{\link{sonify}} for general details.
}

