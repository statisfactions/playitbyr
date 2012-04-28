\name{scale_pan_continuous}
\alias{scale_pan_continuous}
\alias{scale_pan_exp}
\title{Continuous pan scales}
\usage{
  scale_pan_continuous(soundlimits, limits = NULL,
    by = NULL, ...)

  scale_pan_exp(soundlimits, limits = NULL, by = NULL, ...)
}
\arguments{
  \item{\dots}{Other parameters (currently ignored)}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}

  \item{by}{The unit to round the sound parameter to.}
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
  parameters (here, \code{"pan"}, the left-right balance of
  the note as a number between 0 and 1).
}
\details{
  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{shape_scatter}}, which has more information
  about this parameter
}

