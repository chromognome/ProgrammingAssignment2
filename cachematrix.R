## This script contains R functions that are able to cache potentially time-consuming
## computations. If the contents of an object are not changing, it may make sense to
## cache the value of an operation so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## The following function produces a list object containing four functions:
## 1) set()
## 2) get()
## 3) setInverse()
## 4) getInverse()
## to manipulate the values of local variables in the environment defined by
## 'makeCacheMatrix' which remains in memory. The two set functions use the '<<-'
## assignment operator to control the values of variables in the parent environment.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(x1) {
    x <<- x1
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(invM) i <<- invM
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function computes the inverse of the formal variable (a matrix)
## defined in the 'makeCacheMatrix' object above. If the inverse has already been
## calculated (and the matrix has not changed), then 'cacheSolve' retrieves the
## cached value instead.


cacheSolve <- function(y, ...) {
  j <- y$getInverse()
  if(!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- y$get()
  j <- solve(data, ...)
  y$setInverse(j)
  j
}


## Demonstrate use of the functions.

## Create special 'matrix' object.
M <- makeCacheMatrix()

## Define matrix and use the functions of the special 'matrix' object. Show that this
## object is incomplete until 'cacheSolve' is called, and that the cached 'solve'
## result is subsequently used.

M$set(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
M$get()
M$getInverse()
cacheSolve(M)
cacheSolve(M)

## Change matrix and show that the inverse matrix is recalculated then cached.

M$set(matrix(c(3, -2, -1, 0), nrow = 2, ncol = 2))
M$get()
M$getInverse()
cacheSolve(M)
cacheSolve(M)
