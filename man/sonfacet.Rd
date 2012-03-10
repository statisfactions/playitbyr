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
  \code{facet_wrap}.
}
\details{
  When added onto a \code{sonify} object, this function
  splits the data by the variable given in \code{facets}
  and creates separate sonifications for each subset of the
  data. All factor levels not used in the data will
  automatically be dropped.

  Unlike \code{facet_wrap}, this function does not take a
  formula--instead it only takes the name of the data
  variable to split by.
}

