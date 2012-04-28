\name{sonsave}
\alias{sonsave}
\title{Save sonifications to an audio file}
\usage{
  sonsave(what, where, play = FALSE, out = "dac", ...)
}
\arguments{
  \item{what}{A sonify object}

  \item{where}{The path to the desired \code{.wav} output.}

  \item{play}{Play the resulting file after saving?}

  \item{out}{If playing, this gives the output channel.
  Usually users will want to leave this as \code{"dac"},
  the default (which plays to the default audio out).}

  \item{\dots}{other arguments to be passed onto the
  rendering.}
}
\value{
  Returns the filename of the saved file.
}
\description{
  Convenience function to save a \code{\link{sonify}}
  object to an audio (\code{.wav}) file
}

