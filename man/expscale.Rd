\name{exp_scale}
\alias{exp_scale}
\title{Create a exponential parameter scaling with given minimum and maximum}
\usage{
  exp_scale(x, min, max)
}
\arguments{
  \item{x}{A numeric vector or matrix}

  \item{min}{The desired minimum value, a \code{numeric} of
  length 1. Must be greater than zero.}

  \item{max}{The desired maximum value, a \code{numeric} of
  length 1. Must be greater than zero.}
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
exp_scale(x, min=-2, max=-1.5)

## If max<min, it's rescaled in reverse:
exp_scale(x, min=10, max=1)
}
\seealso{
  \code{\link{sonscaling}}
}

