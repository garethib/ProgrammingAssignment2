## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). 

## This R script file contains a function that creates a special "matrix"
## object that can cache its inverse.

## We assume the matrix is invertible.  An error will be returned otherwise.

## The first function, `makeCacheMatrix` creates a special "matrix", 
## which is a list containing the following functions:
## 1.  set = sets the matrix
## 2.  get = gets the matrix
## 3.  getinverse = gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    getinverse <- function() { 
        if(is.null(minv)) {
            minv <<- solve(x)
        } else message("getting cached data")
        minv
    }
    
    list(set = set, get = get, getinverse = getinverse)
}

