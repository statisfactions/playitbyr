\name{addDataToSonify}
\alias{\%+\%}
\alias{addDataToSonify}
\title{Add a data.frame to a \code{sonify} object}
\usage{
  x \%+\% y

  addDataToSonify(x, y)
}
\arguments{
  \item{x}{A \code{sonify} object whose default
  \code{data.frame} is to be changed}

  \item{y}{A \code{data.frame} to add to \code{x}}
}
\value{
  A \code{sonify} object with \code{y} now as the default
  \code{data.frame}
}
\description{
  This allows you to add a \code{data.frame} onto a
  \code{sonify} object with a syntax analogous to
  \pkg{ggplot2}
}
\details{
  It would be easier if this operation could be done with
  \sQuote{+}, but \code{R} does not allow it.
}
\note{
  \code{\%+\%} conflicts with the \code{\%+\%} defined in
  the \pkg{ggplot2} package. Thus, the identical
  addDataToSonify is provided as a workaround if you have
  both packages loaded.
}
\seealso{
  \code{+.sonify}
}

