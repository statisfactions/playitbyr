\name{scale_tempo_exp}
\alias{scale_tempo_continuous}
\alias{scale_tempo_exp}
\title{Continuous tempo scales}
\usage{
  scale_tempo_exp(soundlimits, limits = NULL, ...)

  scale_tempo_continuous(soundlimits, limits = NULL, ...)
}
\arguments{
  \item{\dots}{Other parameters (currently ignored)}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}
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

