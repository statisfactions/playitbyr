##' Simple faceting
##'
##' Allows creation of \sQuote{facets} for a sonification, i.e. to
##' separately sonify different groups of a dataset, one after another.
##' Analogous to \pkg{ggplot2}'s \code{facet_wrap}.
##'
##' When added onto a \code{sonify} object, this function splits the
##' data by the variable given in \code{facets} and creates separate
##' sonifications for each subset of the data. All factor levels not used in
##' the data will automatically be dropped.
##' 
##' Unlike \code{facet_wrap}, this function does not take a
##' formula--instead it only takes the name of the data variable to
##' split by.
##'
##' @param facet Variable to split by
##' @param scales Should scales be fixed (\code{"fixed"}, the
##' default), or free (\code{"free"})
##' @param pause Length of pause, in seconds, between each facet
sonfacet <- function(facet = NULL, scales = "fixed", pause = 0.5) {
  ## Deparse (convert to string) any unquoted string arguments, which R
  ## interpreted as mode "name" in above:
  if(is.name(substitute(facet)))
    facet <- deparse(substitute(facet))

  structure(list(facet = facet, scales = scales, drop = drop, pause = pause), class = "sonfacet")
}



