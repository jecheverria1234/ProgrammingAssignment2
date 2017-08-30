# Project Goal
# Author: Juan Echeverria
#
#
# Matrix inversion is usually a costly taks and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly 
# This script contains  a pair of functions that cache the inverse of a matrix and retrieve it
# if need it again

# Function: makeCacheMatrix
# This function receives one argument (a matrix)
# and creates a special vector that includes the get and set methods
# associated with object oriented classes.
# It also set the global value of two variables using the special assignment character "<<-"
# which sets the value in the parent environment

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# Function: cacheSolve
# This function returns the inverse of a matrix
# If the inverse was already calculated and stored in cache, it gets the value from cache
# if not, it calculates the inverse and store it on cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinv(inv)
      inv
}
