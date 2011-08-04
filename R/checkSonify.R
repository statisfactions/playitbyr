checkSonify <- function(x) {
  ## This function is called by \code{\link{print.sonify}}
  ## before rendering to check if the object can be rendered; it can also
  ## be called directly by the user for diagnostic purposes.

  
  ## TODO note data check on man page

  xname <- deparse(substitute(x))
  if(!("sonify" %in% class(x)))
    stop("'",xname,"' is not a 'sonify' object.")
  
  ## Do any layers of x contain data?
  layers.null <- all(sapply(x$sonlayers, function(y) is.null(y$data)))
  if(is.null(x$data) & layers.null)
    stop("No data.frame provided for sonification. See ?sonify.")
  
  ## Checks that correct mapping slots are filled  
  map <- .getMappings(x, 1)
  if(!xor(is.null(map$time), is.null(map$tempo)))
    stop("Either 'time' or 'tempo' must be set, but not both. See ?sonaes.\n\n",
         xname, "$mapping$time:  ", as.character(x$mapping$time), "\n",
         xname, "$mapping$tempo: ", as.character(x$mapping$tempo), "\n")
  if(is.null(map$tempo)) mapnames <- setdiff(names(map), c("tempo", "timbre"))
  if(is.null(map$time)) mapnames <- setdiff(names(map), c("time", "timbre"))
  
  nullmaps <- sapply(mapnames, function(y) {
    if(is.null(map[[y]])) return(y) else return(NA)})
  nullmaps <- na.omit(as.vector(nullmaps, "character"))
  nullmaps

  if(length(nullmaps)>0)
    stop("These sonic parameters need to be set to a value or \n",
         "mapped to a data.column (see ?sonaes):\n",
         paste(nullmaps, collapse=", "), ".")

  ## Checks that any non-numeric mappings correspond to a numeric
  ## data column  
  nonnumericmaps <- sapply(mapnames, function(y) {
    if(!is.numeric(map[[y]])) return((y=map[[y]])) else return(NA)})
  nonnumericmapnames <- names(nonnumericmaps)[!is.na(nonnumericmaps)]
  nonnumericmaps <- na.omit(as.vector(nonnumericmaps, "character"))
  if(length(nonnumericmaps)==0)
    stop("No data columns selected as mappings. You need to set\n",
         "at least one data column to be mapped with sonaes().")
  names(nonnumericmaps) <- nonnumericmapnames
  unmatched <- nonnumericmaps[!(nonnumericmaps %in% names(x$data))]
  if(length(unmatched)>1)
    stop("These sonic parameters are set as nonnumeric with sonaes(),\n",
         "so they are assumed to be columns of the data.frame for\n",
         "sonification. BUT, they do not match any columns in the\n",
         "given data.frame:\n\n",
         paste(names(unmatched), ": ", unmatched, sep="", collapse="\n"), "\n\n",
         "data.frame column names:\n", paste(names(x$data), collapse="\n"))

  datatest <- sapply(na.omit(nonnumericmaps), function(y) {
    is.numeric(x$data[,y])})
  if(any(!datatest))
    stop("Non-numeric columns of the dataset cannot be used in a mapping.\n\n",
         "These columns are non-numeric and are used as mappings:\n",
         paste(nonnumericmaps[!datatest], collapse="\n"))

  ## Check that all non-numeric mappings have an associated scaling
  scalesnull <- sapply(nonnumericmapnames, function(y) {
    is.null(x$scales[[y]])})
  if(any(scalesnull))
    stop("Every mapping of a sonic parameter to a data column must also\n",
         "have an associated scale.\n\n",
         "Sonic parameters with missing scales:\n",
         paste(nonnumericmapnames[scalesnull], collapse="\n"))
}                               
