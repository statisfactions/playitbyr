\name{scaleShortcuts}
\alias{scaleShortcuts}
\alias{scale_dur_linear}
\alias{scale_pan_linear}
\alias{scale_pitch_linear}
\alias{scale_tempo_linear}
\alias{scale_time_linear}
\alias{scale_vol_linear}
\title{Shortcut functions to set linear scales for \code{sonify}
  objects}
\usage{
scale_pitch_linear(min, max)
scale_time_linear(min, max)
scale_tempo_linear(min, max)
scale_dur_linear(min, max)
scale_pan_linear(min, max)
scale_vol_linear(min, max)
}
\description{

  These shortcut functions (with the notation \code{scale_}\kbd{[data
  parameter]}\code{_}\kbd{[scaling function]}) are intended to be added
  onto a \code{sonify} object to specify how \kbd{[data parameter]} from
  the data (a column of the data) is to be mapped to sonic parameters.

}
\arguments{
  \item{min}{The desired minimum value, a \code{numeric} of length 1}
  \item{max}{The desired maximum value, a \code{numeric} of length 1}
}
\value{
  A \code{sonscaling} object, to be used in \code{sonify} or added onto
  a \code{sonify} object.
}

\seealso{
  \code{\link{sonscaling}} for the full syntax and range of
  possibilities for specifying scaling of \code{sonify} objects;
  \code{\link{linear.scale}} for the pre-defined linear scaling
  function which this incorporates; \code{\link{+.sonify}} for the
  addition onto \code{sonify} objects.
}

\details{

  The syntax of specifying a full \code{sonify} object can be rather
  verbose. Inspired by the \pkg{ggplot2} package, \pkg{playitbyr}
  provides shortcut functions to make the process easier and more
  intuitive.

  These functions are intended to be added onto a pre-existing sonify
  object, as shown in the examples.
}

\examples{
## A verbose way of specifying a sonify object,
## without using the shortcuts
x <- sonify(data=iris, 
     mapping = sonaes(time="Petal.Width",
       pitch = "Petal.Length", 
       timbre=1, dur=5, vol=0.75),
     scales = sonscaling(
       time = list(min=0, max=3,
        scaling.function = linear.scale),
       pitch = list(min=3, max=13,
        scaling.function = linear.scale)))
summary(x)

## An equivalent and much more compact way, with
## the shortcut functions
y <- sonify(data=iris, 
      sonaes(time="Petal.Width",
      pitch = "Petal.Length", 
      timbre=1, dur=5, vol=0.75))
y <- y + scale_time_linear(0, 3)
y <- y + scale_pitch_linear(3, 13)
summary(y)

## You can also change a mapping incrementally.
## This will make the sonification 12 seconds
## long instead of 3
y <- y + scale_time_linear(0,12)
summary(y)
}