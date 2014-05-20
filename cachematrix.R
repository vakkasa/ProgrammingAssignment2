## Cache the potentially time-consuming inverse computation of a matrix based on
## the caching of the mean of a vector given on Coursera's R Programming Assignment 2

## The two functions work together in order to achieve the needed result
## We can test the below functions with
## source('cachematrix.R')
## z<-makeCacheMatrix(matrix(rnorm(9),3,3))
## cacheSolve(z) twice to get the cached data


## makeCacheMatrix wraps a square matrix in order to give the extra fields needed
## for the caching capability of cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # this is called from cacheSolve in order to store the cached object
    setinverse <- function(solve) m <<- solve 
    
    # this is called from cacheSolve in order to retrieve the cached object, if available
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve take a makeCacheMatrix object and invert it
## If it is already inverted it returns a cached copy

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    # if cached inversed matrix exists return that instead
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # compute the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}