## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). 

## This R script file contains a pair of functions that
## cache the inverse of a matrix:  
##
## 1.  `makeCacheMatrix`: creates a special "matrix" object
## that can cache its inverse.
##
## 2.  `cacheSolve`: computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 

## We assume the matrix is invertible.  An error will be returned otherwise.

## The first function, `makeCacheMatrix` creates a special "matrix", 
## which is a list containing the following functions:
## 1.  set = sets the matrix
## 2.  get = gets the matrix
## 3.  setinverse = sets the inverse of the matrix
## 4.  getinverse = gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinv) minv <<- matrixinv
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## `cacheSolve`: computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}
