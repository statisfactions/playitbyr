\name{scale_time_continuous}
\alias{scale_time_continuous}
\alias{scale_time_exp}
\title{Continuous scales for note starting times}
\usage{
  scale_time_continuous(soundlimits, limits = NULL,
    by = NULL, ...)

  scale_time_exp(soundlimits, limits = NULL, by = NULL,
    ...)
}
\arguments{
  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{by}{The unit to round the sound parameter to.}

  \item{\dots}{Other parameters (currently ignored)}
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
  parameters (here \code{"time"}, the starting time of the
  note in seconds).
}
\details{
  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{shape_scatter}}, which has more information
  about this parameter
}

