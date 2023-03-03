## This script contains R functions that are able to cache potentially time-consuming
## computations. If the contents of an object are not changing, it may make sense to
## cache the value of an operation so that when we need it again, it can be looked up
## in the cache rather than recomputed

rm(list=ls())

## The following function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(invM) i <<- invM
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function computes the inverse of the special 'matrix' returned by
## 'makeCacheMatrix' above. If the inverse has already been calculated (and the matrix
## has not changed), then 'cacheSolve' retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## Demonstrate use of the functions

## Create special 'matrix' object
M <- makeCacheMatrix()

## Define matrix and use the functions of the special 'matrix' object. Show that this
## object is incomplete until 'cacheSolve' is called and that the cached 'solve'
## result is subsequently used

M$set(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
M$get()
M$getInverse()
cacheSolve(M)
cacheSolve(M)

