\name{scale_tempo_exp}
\alias{scale_tempo_continuous}
\alias{scale_tempo_exp}
\title{Continuous tempo scales}
\usage{
  scale_tempo_exp(min, max, dmin = NULL, dmax = NULL, ...)

  scale_tempo_continuous(min, max, dmin = NULL,
    dmax = NULL, ...)
}
\arguments{
  \item{\dots}{Other parameters (currently ignored)}

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
}
\value{
  A \code{sonscaling} object, to be used in \code{sonify}
  or added onto a \code{sonify} object.
}
\description{
  These shortcut functions (with the notation
  \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
  function]}) are intended to be added onto a \code{sonify}
  object to specify how \kbd{[data parameter]} from the
  data (a column of the data) is to be mapped to sonic
  parameters (here, \code{"tempo"}).
}
\details{
  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{sonscaling}} for the full syntax and range of
  possibilities for specifying scaling of \code{sonify}
  objects; \code{\link{linear_scale}}
  \code{\link{exp_scale}} for the pre-defined scaling
  functions which this incorporates; \code{\link{+.sonify}}
  for the addition onto \code{sonify} objects.
}

