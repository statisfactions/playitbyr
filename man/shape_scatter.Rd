\name{shape_scatter}
\alias{shape_scatter}
\title{Create a scatterplot layer}
\usage{
  shape_scatter(jitter = 0, relative = TRUE, ...,
    data = NULL, mapping = NULL)
}
\arguments{
  \item{jitter}{The maximum size, in seconds, of how much
  to jitter time by when there are multiple notes at the
  same pitch and time (the sonic equivalent of
  overplotting). The default, 0, means no jitter occurs.}

  \item{relative}{Make the duration relative to the overall
  length of the sonification? The default, \code{TRUE},
  means that the sonification will scale durations relative
  to the length of the overall sonification (it estimates
  how long a \dQuote{beat} is and then rescales duration in
  relation to that). Otherwise, durations remain constant
  even if the sonification is much longer or shorter (which
  may mean that note durations must be fiddled with so they
  don't overlap).}

  \item{\dots}{settings to pass to \code{\link{sonlayer}}
  (see Details)}

  \item{data}{The \code{data.frame} to be sonified for this
  layer. If blank, the data from the parent \code{sonify}
  object is used.}

  \item{mapping}{A \code{\link{sonaes}} object.}
}
\value{
  A \code{sonlayer} object that can be added onto a
  \code{\link{sonify}} object.
}
\description{
  Creates a scatterplot layer; the audio analogue of
  \pkg{ggplot2}'s \code{geom_point}.
}
\details{
  The audio scatterplot is implemented by a simple
  frequency-modulation synthesis (through Csound). The
  following parameters are available for setting or
  mapping:

  \describe{ \item{time}{The starting time of the note (in
  seconds).} \item{pitch}{The pitch of the note, in the
  Csound
  \href{"http://www.csounds.com/manual/html/cpsoct.html"}{oct
  notation} notation for pitches, where 8 is middle C and 1
  represents an octave, to the corresponding frequency in
  Hertz.  By default this is scaled to the nearest musical
  (chromatic) pitch. (See
  \code{\link{scale_pitch_continuous}}.)} \item{dur}{The
  duration of the note (relative to the total time if
  \code{relative = TRUE}, in seconds otherwise).}
  \item{amp}{The volume of the note, as a proportion
  between 0 and 1, where 1 is the maximum volume. Note that
  a multiple notes that happen at the same time could add
  up to more than one, causing distortion and clipping.}
  \item{attkp}{The proportion of the note's length devoted
  to the initial (linear) attack.} \item{decayp}{The
  proportion of the note's length devoted to the (linear)
  decay.} \item{indx}{The index of modulation. This affects
  the distortion of the tone; \code{indx = 0} is a sine
  wave, whereas higher indices of modulation give
  increasingly complex tones.} \item{mod}{The modulating
  frequency, given as a multiple of the primary frequency
  (i.e. given by \code{pitch}).} }

  To \emph{set} a sound parameter to a value, you simply
  include it as an extra argument in \code{shape_scatter};
  to \emph{map} a parameter, you set the mapping for the
  layer or the \code{sonify} object using
  \code{\link{sonaes}} (see examples).
}
\examples{
x1 <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter() # no jitter
\dontrun{x1}
x2 <- sonify(iris, sonaes(time = Petal.Width, pitch = Petal.Length)) +
  shape_scatter(jitter = 0.3) # substantial jitter, fuzzes out overlap
\dontrun{x2}

## relative = TRUE: rescales duration to fit overall length (usually easier to hear)
d <- cbind(airquality, row = rownames(airquality))
x3 <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 10))
\dontrun{x3}
x4 <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(dur = 3) +
  scale_time_continuous(c(0, 5))
\dontrun{x4}

## relative = FALSE: duration is in seconds and is not scaled to fit overall length
## (creates lots of overlap)
x5 <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 10))
\dontrun{x5}
x6 <- sonify(d, sonaes(time = row, pitch = Temp)) + shape_scatter(relative = FALSE, dur = 3) +
  scale_time_continuous(c(0, 5))
\dontrun{x6}

## Setting the pitch equal to 8 (C), and using iris$Sepal.Width
## to generate the timings of notes##'
x7 <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 9)
\dontrun{x7}

## Instead, /mapping/ the pitch to 9.
x8 <- sonify(iris[1:10,], sonaes(time = Sepal.Width)) + shape_scatter(pitch = 9)
\dontrun{x8}
## If a value in the mapping
## is a vector and is not a name of the data column, playitbyr
## creates a new column with that value. This is then scaled,
## producing an unexpected F# (8.5) here when you might expect the same
## sound as above!
}

