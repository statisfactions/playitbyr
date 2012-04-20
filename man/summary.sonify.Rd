\name{summary.sonify}
\alias{summary.sonify}
\title{Summary method for sonify objects}
\usage{
  \method{summary}{sonify} (object, ...)
}
\arguments{
  \item{object}{A \code{sonify} object}

  \item{\dots}{Ignored.}
}
\value{
  \code{summary.sonify} prints a brief summary, including
  info about the dataset to be sonified, and the specified
  mappings and scalings of sonic values to data parameters.
}
\description{
  Summarizes the structure of a sonify object in ways
  useful for seeing what the output will be.
}
\examples{
summary(sonify(iris, sonaes(time = Sepal.Length, pitch = Sepal.Width)) +
        shape_scatter() + shape_dotplot())
}
\author{
  Closely based on \code{summary.ggplot} by Hadley Wickham
}
\seealso{
  \code{\link{sonify}} for the creation of these objects
}

