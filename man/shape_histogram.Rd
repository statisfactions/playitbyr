\name{shape_histogram}
\alias{shape_histogram}
\title{Create a histogram layer}
\usage{
  shape_histogram(length = 5, tempo = 240, ...)
}
\arguments{
  \item{length}{The overall length, in seconds, of the
  histogram.}

  \item{tempo}{The tempo, in beats per minute; i.e. the
  rapidity with which to represent values from the
  dataset.}

  \item{\dots}{data, settings, and mappings to pass to
  \code{\link{sonlayer}}}
}
\value{
  A \code{sonscaling} object
}
\description{
  Creates a histogram layer
}
\details{
  The audio histogram is implemented by a simple
  frequency-modulation synthesis (through csound),
  representing a univariate view of the data by rapidly
  playing pitches that are samples from the dataset. Only
  \code{pitch} is intended to be used for mapping but the
  remainder of the same parameters available in
  \code{shape_scatter} are available for setting to suit
  the analyst's ear:

  \describe{ \item{time}{The starting time of the note (in
  seconds).} \item{pitch}{The pitch of the note, in the
  Csound
  \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
  notation} notation for pitches, where 8 is middle C and 1
  represents an octave, to the corresponding frequency in
  Hertz.} \item{dur}{The duration of the note (in
  seconds).} \item{amp}{The volume of the note, as a
  proportion between 0 and 1, where 1 is the maximum
  volume. Note that a multiple notes that happen at the
  same time could add up to more than one, causing
  distortion an clipping.} \item{attkp}{The proportion of
  the note's length devoted to the initial (linear)
  attack.} \item{decayp}{The proportion of the note's
  length devoted to the (linear) decay.} \item{mod}{The
  modulating frequency, given as a \emph{multiple} of the
  carrier tone.} \item{indx}{The index of modulation.} }

  To \emph{set} a sound parameter to a value, you simply
  include it as an extra argument in \code{shape_scatter};
  to \emph{map} a parameter, you set the mapping for the
  layer or the \code{sonify} object using
  \code{\link{sonaes}} (see examples).
}

