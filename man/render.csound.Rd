\name{render.csound}
\alias{csound_layer}
\alias{csound_layer.scatter}
\alias{render.csound}
\title{Rendering sound using csound}
\usage{
  render.csound(x, opts, audioSample = FALSE, ...)

  csound_layer(sonlayerscore, ...)

  \method{csound_layer}{scatter} (sonlayerscore, ...)
}
\arguments{
  \item{\dots}{Currently ignored}

  \item{sonlayerscore}{An element of the score list--the
  score produced for a specific layer. The class of this
  determines the shape to be rendered}

  \item{x}{A \code{score} object created by
  \code{\link{.getScore}}}

  \item{opts}{the options of the original sonify object}

  \item{audioSample}{Logical indicating whether to return
  an \code{audioSample} object containing the from the
  object.}
}
\description{
  \code{render.csound} uses the csound package to render a
  \code{score} object.
}
\note{
  See \code{\link{createPerformance}} for possible
  \code{render_options} that can be used with this
  rendering technique; all the arguments of
  \code{createPerformance} can be passed as options.
}

