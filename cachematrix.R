## General description of the exercise. Programming assignment 2
## Functions to store a matrix and cache its inverse to save time
## when the inverse is needed repeatedly.
## Gumer Freire. Coursera data science R programming, februrary 2021.

## makeCacheMatrix creates a special matrix object that can store its inverse
## when calculated for the first time.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) m <<- inversematrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a square matrix. If it was calculated
## before, it returns the cached inverse. If it is not available, it calculates
## the inverse, stores it in cache and returns the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
