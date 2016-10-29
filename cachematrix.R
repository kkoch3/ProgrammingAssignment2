## Ken Koch - Assignment 2
## This program creates a matrix object and caches its inverse

## This function creates defines the cached matrix object and allows its
## value and its inverse to be set and retrieved for future use without
## requiring a recalculation of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks to see if a cached version of the matrix's inverse
## already exists. If it does, it uses it. If not, it creates it and 
## caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {  #The cached inverse exists, let's get it
    message("getting cached data")
    return(inverse)
  }
  # The cached inverse doesn't exist, let's get the cached matrix, compute
  # its inverse via solve and then cache it.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
