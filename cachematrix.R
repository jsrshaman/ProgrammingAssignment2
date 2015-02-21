## A pair of functions that cache and compute the inverse of a matrix.
## The following creates a special matrix object that can cache its
## inverse.

makeCacheMatrix <- function(z = matrix()) {
      inverse <- NULL
      set <- function(x) {
            z <<- x;
            inverse <<- NULL;
      }
      get <- function() return(z);
      setinv <- function(inv) inverse <<- inv;
      getinv <- function() return(inverse);
      return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This one computes the inverse and if it has been already calculated
## cacheSolve should retrieve the inverse.

cacheSolve <- function(z, ...) {
      inverse <- z$getinv()
      if(!is.null(inverse)) {
            message("Retrieving cached data...")
            return(inverse)
      }
      data <- z$get()
      invserse <- solve(data, ...)
      z$setinv(inverse)
      return(inverse)
}
