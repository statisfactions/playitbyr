\name{internalrender}
\alias{.createNoteAudio}
\alias{internalrender}
\alias{render.audio}
\title{Internal functions to generate sound from a \code{score} object.}
\usage{
  \method{render}{audio} (x, audioSample = FALSE, ...)

  .createNoteAudio(noterow, samp.rate)
}
\arguments{
  \item{x}{A \code{sonify} object}

  \item{noterow}{A row of the \code{data.frame} returned by
  \code{.getNotes}, spoon-fed to \code{.createNoteAudio}
  one by one by \code{render.audio}}

  \item{samp.rate}{The sampling rate, in Hertz}

  \item{audioSamp}{The \code{audioSample} object to be
  played}
}
\value{
  \code{render.audio} returns an \code{audioSample} object
  (from the \code{audio} package).

  \code{.createNoteAudio} creates each individual note one
  by one for each row returned by \code{.getNotes}.

  \code{play_audioSample} is called for its side effect, to
  produce the sound of the sonification.
}
\description{
  These functions are not intended to be called directly by
  the user.  \code{render} is a generic that takes a
  \code{sonify} object and determines which method to call
  based on the class of the object; play_audioSample plays
  the resulting \code{audioSample} object.
}
\details{
  \code{render.audio} (the only currently available
  rendering method), calls \code{.createNoteAudio} for
  every note in the data frame returned by
  \code{.getScore}. Finally, \code{play_audioSample} is
  called to produce sound.
}
\note{
  This file is currently under heavy development and will
  be changing drastically in the development version.
}
\keyword{internal}

