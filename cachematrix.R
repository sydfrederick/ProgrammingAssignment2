## Submitted by Syd Frederick
## These functions are made to create a matrix
## and cache its own inverse, saving in overall
## computation time when using the inverse
## repeatedly

## This first function creates a "matrix"
## that has the ability to set and get
## both the values of the matrix and
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This second function is meant to compute
## the inverse of a matrix created by 
## makeCacheMatrix and cache the inverse by 
## first checking if the inverse is already 
## cached, and then coputing the inverse if the
## inverse is not cached

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}