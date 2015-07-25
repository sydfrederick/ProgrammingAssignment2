## Submitted by Syd Frederick
## ****PLEASE SEE END OF FILE FOR AN EXAMPLE****

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

##EXAMPLE:
## First create a makeCacheMatrix like this:
## > makeCacheMatrix(x)
## This inititializes the matrix list
## 
## Next, you want to set the data, this 
## is the most crucial step to this function:
## > x$set(matrix(c(1,2,3,4), 2, 2))
##
## Of course, you can use any matrix you want, as long
## as it is square and invertible
##
## Now you can correctly call cacheSolve()
## > cacheSolve(x)
##
## NOTE: If you do not set the data, you have not
## properly created a makeCacheMatrix 