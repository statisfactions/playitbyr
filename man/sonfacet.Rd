\name{sonfacet}
\alias{sonfacet}
\title{Simple faceting}
\usage{
  sonfacet(facet = NULL, scales = "fixed", pause = 0.5)
}
\arguments{
  \item{facet}{Variable to split by}

  \item{scales}{Should scales be fixed (\code{"fixed"}, the
  default), or free (\code{"free"})}

  \item{pause}{Length of pause, in seconds, between each
  facet}
}
\description{
  Allows creation of \sQuote{facets} for a sonification,
  i.e. to separately sonify different groups of a dataset,
  one after another. Analogous to \pkg{ggplot2}'s
  \code{\link[ggplot2]{facet_wrap}}.
}
\details{
  When added onto a \code{sonify} object, this function
  splits the data by the variable given in \code{facets}
  and creates separate sonifications for each subset of the
  data. All factor levels not used in the data will
  automatically be dropped.
}
\note{
  Unlike \code{facet_wrap}, this function does not take a
  formula--instead it only takes the name of the data
  variable to split by.
}
\examples{
df <- data.frame(pits = c(0, 1, 1, 2, 2, 3),
                   tims = c(0, 1, 0, 1, 0, 1),
                   facs = c(0, 0, 1, 1, 2, 2))
x <- sonify(df, sonaes(time = tims, pitch = pits)) + shape_scatter(dur = 1) +
   scale_time_continuous(c(0, 1)) + scale_pitch_continuous(c(8, 8.25)) +
     sonfacet(facs, pause = 1)

\dontrun{x}
}

