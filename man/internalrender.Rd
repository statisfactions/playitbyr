\name{internalrender}
\alias{render}
\alias{render.audio}
\alias{playAudioRendering}
\title{Internal functions to generate sound from a
  \code{sonify} object.}
\description{These functions are not intended to be called directly by
  the user. \code{render} is a generic that takes a \code{sonify} object
  and determines which method to call based on the class of the object;
  playAudioRendering plays the resulting \code{audioSample} object.}
\usage{
render(s)
render.audio(s)
.createNote(noterow, samp.rate)
playAudioRendering(audioSamp)
}
\value{
  \code{render.audio} returns an \code{audioSample} object (from the
  \code{audio} package).

  \code{.createNote} creates each individual note one by one for each
  row returned by \code{.getNotes}.

  \code{playAudioRendering} is called for its side effect, to produce
  the sound of the sonification.}
\arguments{
  \item{s}{A \code{sonify} object}
  \item{noterow}{A row of the \code{data.frame} returned by
    \code{.getNotes}, spoon-fed to \code{.createNote} one by one by
    \code{render.audio}}
  \item{samp.rate}{The sampling rate, in Hertz}
  \item{audioSamp}{The \code{audioSample} object to be played}
}

\details{
  \code{render} is called by \code{print.sonify}; it then calls
  \code{render.audio} (the only currently available rendering method),
  which calls \code{.createNote} for every note in the data
  frame returned by \code{.getNotes}. Finally, \code{playAudioRendering} is called to produce sound.}

\keyword{internal}
