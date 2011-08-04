sonaes <- function(time=0, pitch=8, dur=2, vol=1, pan=0.5, tempo=NULL, timbre="sine") {
  ##Similar to ggplot2 "aes", for generating mappings of data to sound.
  ## 'sonaes' objects are lists and are used as the top-level 'mapping' of sonify objects
  
  if(!missing(time) && !missing(tempo))
    stop("Only one of 'time' or 'tempo' can be provided.")

  given <-  as.list(match.call()[-1])
  ## Deparse any unquoted data.frame columns given as args
  given <- lapply(given, function(x) {
                  x <- ifelse(is.symbol(x), deparse(x), x)
                  if(!is.null(x)) attr(x, "default") <- FALSE
                  return(x)})
  son <- lapply(formals(), function(x){
                if(!is.null(x)) attr(x, "default") <- TRUE
                return(x)})
  son[match(names(given), names(son))] <- given

  ## Auto-adjust for one of time, tempo being not given
  if(missing(time) && !missing(tempo))
    son["time"] <- list(NULL)
  if(!missing(time) && missing(tempo))
    son["tempo"] <- list(NULL)

  ## Check validity
  if(!is.null(son$time) && son$time < 0)
    stop("time must be greater than 0.")
  if(!is.null(son$tempo) && (son$tempo<=0))
    stop("tempo must be greater than 0 (bpm)")
  if(son$dur<0)
    stop("dur cannot be negative")
  if(((son$vol<0) || (son$vol>1)))
    stop("vol must be between 0 and 1.")
  if((son$pan<0) || (son$pan>1))
    stop("pan must be between 0 and 1.")
  if(son$timbre != "sine")
    stop("'sine' is the only supported timbre right now")  

  class(son) <- c("sonaes")
  son
}
