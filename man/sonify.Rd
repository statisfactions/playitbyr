\name{sonify}
\alias{sonify}
\title{Create sonification objects}
\usage{
  sonify(data = NULL, mapping = sonaes(),
    scales = sonscaling(), sonlayers = NULL,
    opts = sonopts(rendering = "csound"), sonfacet = NULL)
}
\arguments{
  \item{data}{a \code{data.frame} with the data to be
  sonified}

  \item{mapping}{a \code{sonaes} object (created by
  \code{sonaes()}) that maps data to sound parameters}

  \item{scales}{a \code{sonifyScale} object (created by
  \code{scaling()}) that gives the scaling function and
  desired parameter range from the data to each sound}

  \item{sonlayers}{A single \code{sonlayer} object, or a
  list of \code{sonlayer} objects, that gives each layer to
  be rendered.}

  \item{opts}{A named list of rendering options to pass to
  the individual rendering type.}

  \item{sonfacet}{A \code{\link{sonfacet}} object that
  gives what variables to split by}
}
\value{
  A \code{sonify} object, which contains what is needed to
  render the object. If the object is completely specified,
  it can by rendered simply by calling the print method for
  the object (i.e. simply typing the name of the object at
  the R prompt).
}
\description{
  Creates a \code{sonify} object, which is a list
  containing the \code{data.frame} to be sonified, the
  mappings of data to sound parameters, the scaling of
  parameters, and additional options.
}
\examples{
## A bare-bones sonify object, but one that cannot render
x <- sonify()
summary(x)
\dontrun{x # Throws an error message}

## Creating a sonification incrementally
## and with the "scale_" convenience functions
x <- sonify(iris)
x <- x + sonaes(time=Petal.Length, pitch=Petal.Width)
x <- x + shape_scatter()
x <- x + scale_pitch_continuous(c(6, 8)) + scale_time_continuous(c(0, 10))
summary(x)
\dontrun{x # outputs sound}
}
\references{
  The syntax of \code{sonify} objects is heavily based on
  that of \code{ggplot} objects in the \pkg{ggplot2}
  package. See the \href{http://had.co.nz/ggplot2/}{ggplot2
  web site} for more information.
}
\seealso{
  \code{\link{+.sonify}} for interacting with \code{sonify}
  objects; \code{\link{sonaes}} for setting the mappings;
  \code{\link{sonscaling}} for creating the scalings.
}

