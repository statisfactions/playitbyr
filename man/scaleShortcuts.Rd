\name{scale_time_continuous}
\alias{scale_attkp_continuous}
\alias{scale_attkp_exp}
\alias{scale_attkp_identity}
\alias{scale_decayp_continuous}
\alias{scale_decayp_exp}
\alias{scale_decayp_identity}
\alias{scale_dur_continuous}
\alias{scale_dur_exp}
\alias{scale_dur_identity}
\alias{scale_indx_continuous}
\alias{scale_indx_exp}
\alias{scale_indx_identity}
\alias{scale_mod_continuous}
\alias{scale_mod_exp}
\alias{scale_mod_identity}
\alias{scale_pan_continuous}
\alias{scale_pan_exp}
\alias{scale_pan_identity}
\alias{scale_pitch_continuous}
\alias{scale_pitch_exp}
\alias{scale_pitch_identity}
\alias{scaleShortcuts}
\alias{scale_tempo_continuous}
\alias{scale_tempo_exp}
\alias{scale_tempo_identity}
\alias{scale_time_continuous}
\alias{scale_time_exp}
\alias{scale_time_identity}
\alias{scale_vol_continuous}
\alias{scale_vol_exp}
\alias{scale_vol_identity}
\title{Shortcut functions to set linear_scales for \code{sonify} objects}
\usage{
  scale_time_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_pitch_continuous(min, max, dmin = NULL,
    dmax = NULL, chromatic = TRUE, ...)

  scale_dur_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_vol_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_pan_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_tempo_continuous(min, max, dmin = NULL,
    dmax = NULL, ...)

  scale_attkp_continuous(min, max, dmin = NULL,
    dmax = NULL, ...)

  scale_decayp_continuous(min, max, dmin = NULL,
    dmax = NULL, ...)

  scale_mod_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_indx_continuous(min, max, dmin = NULL, dmax = NULL,
    ...)

  scale_time_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_pitch_exp(min, max, dmin = NULL, dmax = NULL,
    chromatic = TRUE, ...)

  scale_dur_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_vol_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_pan_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_tempo_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_attkp_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_decayp_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_mod_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_indx_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_time_identity(...)

  scale_pitch_identity(chromatic = FALSE, ...)

  scale_dur_identity(...)

  scale_vol_identity(...)

  scale_pan_identity(...)

  scale_tempo_identity(...)

  scale_attkp_identity(...)

  scale_decayp_identity(...)

  scale_mod_identity(...)

  scale_indx_identity(...)
}
\arguments{
  \item{min}{The desired minimum value, a \code{numeric} of
  length 1}

  \item{max}{The desired maximum value, a \code{numeric} of
  length 1}

  \item{dmin}{The data value to be lined up with the
  \code{min} argument. This is useful for when you want to
  specify a fixed scale}

  \item{dmax}{The data value to be lined up with the
  \code{max} argument. This is useful for when you want to
  specify a fixed scale}

  \item{chromatic}{For pitches, should they be rounded to
  chromatic values? Default \code{TRUE}}

  \item{\dots}{Other parameters (ignored)}
}
\value{
  A \code{sonscaling} object, to be used in \code{sonify}
  or added onto a \code{sonify} object.
}
\description{
  These shortcut functions (with the notation
  \code{scale_}\kbd{[data parameter]}\code{_}\kbd{[scaling
  function]}) are intended to be added onto a \code{sonify}
  object to specify how \kbd{[data parameter]} from the
  data (a column of the data) is to be mapped to sonic
  parameters.
}
\details{
  The syntax of specifying a full \code{sonify} object can
  be rather verbose. Inspired by the \pkg{ggplot2} package,
  \pkg{playitbyr} provides shortcut functions to make the
  process easier and more intuitive.

  These functions are intended to be added onto a
  pre-existing sonify object, as shown in the examples.
}
\examples{
## A verbose way of specifying a sonify object,
## without using the shortcuts
x <- sonify(data=iris,
     sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75),
     scales = sonscaling(
       time = list(min=0, max=3,
        scaling.function = linear_scale),
       pitch = list(min=3, max=13,
        scaling.function = linear_scale)),
     sonlayers = shape_scatter())
summary(x)
\dontrun{x}

## An equivalent and much more readable way, with
## the shortcut functions
y <- sonify(iris, sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75))
y <- y + shape_scatter()
y <- y + scale_time_continuous(0, 3)
y <- y + scale_pitch_continuous(3, 13)
summary(y)
\dontrun{y}

## You can also change a mapping incrementally.
## New parameters overwrite old ones, so now
## the sonification is stretched to 12 seconds
## long instead of 3, and the pitch is mapped to
## a much narrower range, the octave below tuning A (440 Hz)
y <- y + scale_time_continuous(0, 12) + scale_pitch_continuous(7, 8)
summary(y)
\dontrun{y}
}
\seealso{
  \code{\link{sonscaling}} for the full syntax and range of
  possibilities for specifying scaling of \code{sonify}
  objects; \code{\link{linear_scale}} for the pre-defined
  linear scaling function which this incorporates;
  \code{\link{+.sonify}} for the addition onto
  \code{sonify} objects.
}

