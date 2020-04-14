#####################################################################
#
# matrix constructor with a caching inverse method
#
# Addresses 2 things:
#     - cacheSolve() is associated with this special matrix,
#       so the intention that this is for these special matrices is 
#       clearer
#     - an extraneous function to set the inverse within the object 
#       is not needed; the only way the inverse can be changed
#       from outside is to actually set the source matrix
#
#####################################################################

makeCacheMatrix <- function(x = matrix()) {
  e1 <- "Initializer should be of type matrix"
  ix <- NULL
  
  set <- function(y) {
    if (!is.matrix(y))
      stop(e1)
    
    x <<- y
    ix <<- NULL
  }
  
  get <- function() x
  
  cacheSolve <- function(...) {
    if(!is.null(ix)) {
      message("Getting cached data...")
      return(ix)
    }
    ix <<- solve(a = x, ...)
    ix
  }
  
  set(x)
  list(set = set, get = get,
       cacheSolve = cacheSolve)
}