\name{render.audio}
\alias{audio_layer}
\alias{audio_layer.dotplot}
\alias{audio_layer.scatter}
\alias{.createNoteAudio}
\alias{render.audio}
\title{Rendering sound using the audio package}
\usage{
  render.audio(x, opts, audioSample = FALSE, ...)

  audio_layer(sonlayerscore, out, samp.rate, ...)

  \method{audio_layer}{scatter} (sonlayerscore, out,
    samp.rate, ...)

  \method{audio_layer}{dotplot} (sonlayerscore, out,
    samp.rate, ...)

  .createNoteAudio(noterow, samp.rate)
}
\arguments{
  \item{\dots}{Currently ignored}

  \item{sonlayerscore}{An element of the score list--the
  score produced for a specific layer. The class of this
  determines the shape to be rendered}

  \item{out}{A matrix containing the rendered output of any
  previously rendered sonlayers, or a blank matrix large
  enough to contain the output of this one.}

  \item{samp.rate}{The sampling rate, in Hertz}

  \item{noterow}{A row of the \code{data.frame} returned by
  \code{.getScore}, spoon-fed to \code{.createNoteAudio}
  one by one by \code{render.audio}}

  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{opts}{the options of the original sonify object}

  \item{audioSample}{Logical indicating whether to return
  an \code{audioSample} object containing the from the
  object.}
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
  \code{render.audio} calls \code{.createNoteAudio} for
  every note in the data frame returned by
  \code{.getScore}. Finally, \code{play_audioSample} is
  called to produce sound.
}
\note{
  This file is currently under heavy development and will
  be changing drastically in the development version.
}
\keyword{internal}

