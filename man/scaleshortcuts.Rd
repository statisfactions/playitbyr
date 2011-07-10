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
  parameter]}\code{_}\kbd{[scaling function]}) are invoked in the call to
  \code{sonify} or added onto it in order to specify how \kbd{[data
  parameter]} from the data is to be mapped to sonic parameters.
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
  \code{\link{linear.scale}} for the pre-defined linear scaling function.}

