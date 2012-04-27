\name{sonaes}
\alias{sonaes}
\title{Create mappings and parameters for sonification objects}
\usage{
  sonaes(...)
}
\arguments{
  \item{\dots}{Sonic parameter mappings, specific to the
  shape being rendered. All arguments must be named, with
  the argument names sonic parameters and the values either
  numeric values (to set the sonic parameter to that value
  as a constant) or a column of the data.frame that is to
  be sonified (with or without quotes).}
}
\value{
  A \code{sonaes} object, used in or added to a
  \code{sonify} object or \code{sonlayer}.
}
\description{
  Creates a \code{sonaes} object, which is a list
  containing the desired mappings of data columns or
  constants onto each sound parameter. This is intended be
  added to a \code{sonify} object (or included in its
  construction).
}
\details{
  Setting sonic parameters to \code{data.frame} columns is
  the heart of making the sonification. The sound
  parameters that can be the arguments of \code{sonaes}
  depend on the kinds of layers present; see
  \code{\link{sonify}} for a list of the possible kinds of
  layers.
}
\examples{
## Maps Petal.Width onto tempo,
## Sepal.Width onto pitch,
## and sets scalings
## (not terribly informative, since the order is
## arbitrary)
x <- sonify(iris, sonaes(tempo=Petal.Width,
            pitch=Sepal.Width)) + shape_scatter()
summary(x)
\dontrun{x ## Quite long!}

## Use a different tempo scaling than default
## to hear more of the data faster
\dontrun{x + scale_tempo_continuous(c(300, 1000))}

## Map Petal.Width onto 'time' instead
y <- x + sonaes(time=Petal.Width)
summary(y)
\dontrun{y}

## Seems clipped, so increase duration
\dontrun{y + sonaes(dur=4)}
}
\seealso{
  \code{\link{sonify}}. Also, see
  \code{\link[ggplot2]{aes}} from the \pkg{ggplot2}
  package, which inspired this function.
}

