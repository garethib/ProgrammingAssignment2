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
    ## minv stores the inverse; initialise with NULL
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
## "matrix" returned by `makeCacheMatrix` above. 
## First if fetches the cached value.
    ## If this is not NULL, this indicates that the inverse has
    ## already been calculated (and the matrix has not changed), so we
    ## return that cached value.
    ## If this is NULL, then the we call solve() to calculate the inverse
    ## and then set the cache for future use.

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

## Note that the above has been implemented as specified in the assignment.
## My preferred way to deal with this would have been for the "special"
## matrix to contain all necessary functions, and only an externally visible
## getinverse() function.
## getinverse() would check its internal cache and re-calculate if necessary. 
## setinverse() and cacheSolve() would not be necessary.
## The implementation below mirrors solve(m) with cacheSolve(sm) at the expense
## of simplicity