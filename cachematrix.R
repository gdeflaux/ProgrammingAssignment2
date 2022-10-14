## This script allows provide a caching functionality for the computation of the
## inverse of a matrix, i.e. the inverse of a matrix is only computed if it 
## wasn't already.

## This function creates an environment containing variables and functions.
## It returns a list with links to these functions. These functions provide get 
## and set operations for the input matrix (x) and for the inverse of that
## matrix (i). If the input matrix is updated the inverse matrix is reset (NULL).
## This approach emulates the definition of a class with attributes and methods.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of a matrix given that is was created by
## the makeCacheMatrix function. The inverse of the matrix is computed only if 
## it wasn't already (x$getinverse() returns NULL). Otherwise it returns the 
## cached inverse matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
