makeCacheVector <- function(x = numeric()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getmean <- function(...) {
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    m <<- mean(x, ...)
    m
  }
  set(x)
  list(set = set, get = get,
       getmean = getmean)
}