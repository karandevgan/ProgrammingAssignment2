
## makeCacheMatrix function creates a matrix that can cache its inverse
## cacheSolve computes the inverse of the matrix if its not in the cache 
## and returns the inverse from the cache if its available


## makeCacheMatrix takes an invertible matrix and then returns the list of 
## functions
## set function sets the value of matrix 
## get function returns the matrix
## setInverse sets the inverse
## getInverse returns the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve take a makeCacheMatrix object
## It calculates the inverse if its not stored in the cache otherwise returns
## the inverse stored in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Getting cached value")
        return (inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
