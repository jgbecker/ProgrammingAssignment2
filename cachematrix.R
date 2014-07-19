## This file defines two functions.
## makeCacheMatrix : generate a matrix that can cache its inverse
## cacheSolve      : return the (possibly cached) inverse of a matrix generated with makeCacheMatrix

## makeCacheMatrix : generate a matrix that can cache its inverse
## parameter x : the matrix to transform into a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # will end up in the closure and contain the cached inverse; is NULL if none has been cached yet
  inv <- NULL
  
  # function to set a new matrix; clears the cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function to get the matrix
  get <- function() x
  
  # (internal) function to populate the cache
  setinverse <- function(inverse) inv <<- inverse
  
  # (internal) function to retrieve cache contents
  getinverse <- function() inv
  
  # create and return matrix that can cache its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve      : return the (possibly cached) inverse of a matrix generated with makeCacheMatrix
##                     - if the inverse has been cached, return the cached inverse
##                     - if the inverse has not been cached yet, calculate the inverse, cache it, and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get cache contents
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    # something has been cached
    message("getting cached data")
    return(inv)
  }
  
  # nothing has been cached
  # get matrix, calculate inverse, cache inverse, return inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
