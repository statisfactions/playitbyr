\name{exp_scale}
\alias{exp_scale}
\title{Create a exponential parameter scaling with given minimum and maximum}
\usage{
  exp_scale(x, limits = NULL, soundlimits, by = NULL)
}
\arguments{
  \item{x}{A numeric vector or matrix}

  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{by}{The unit to round the sound parameter to. See
  examples.}
}
\value{
  A numeric vector or matrix of the same type as \code{x},
  exponentially rescaled in the desired way.
}
\description{
  \code{exp_scale()} exponentially transforms its input
  vector \code{x} so that its minimum value is \code{min}
  and its maximum value is \code{max}. This is for use as a
  scaling function in \code{\link{sonscaling}}. It is
  unlikely to be directly called by the user.
}
\details{
  This simple function does the important work of actually
  rescaling the given data of a \code{sonify} object into
  the range of the given sonic parameter, which is why it's
  included in \pkg{playitbyr}. But it can be used on any
  vector or matrix.
}
\examples{
x <- 1:10
exp_scale(x, soundlimits = c(-2, -1.5))

## If max<min, it's rescaled in reverse:
exp_scale(x, soundlimits = c(10, 1))
}
\seealso{
  \code{\link{linear_scale}} for more details on how
  scaling functions work
}
\keyword{internal}

