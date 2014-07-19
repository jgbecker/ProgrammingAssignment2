## This file defines two functions.
## makeCacheMatrix : generate a matrix that can cache its inverse
## cacheSolve      : return the (possibly cached) inverse of a matrix generated with makeCacheMatrix
##                     - if the inverse has been cached, return the cached inverse
##                     - if the inverse has not been cached yet

## makeCacheMatrix : generate a matrix that can cache its inverse
## parameter x : the matrix to transform into a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve      : return the (possibly cached) inverse of a matrix generated with makeCacheMatrix
##                     - if the inverse has been cached, return the cached inverse
##                     - if the inverse has not been cached yet, calculate the inverse, cache it, and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
