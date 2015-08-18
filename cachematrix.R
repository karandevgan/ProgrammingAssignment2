
## makeCacheMatrix function creates a matrix that can cache its inverse
## cacheSolve computes the inverse of the matrix if its not in the cache 
## and returns the inverse from the cache if its available



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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Getting cached value")
        return (inverse)
    }
    data <- x$get
    inverse <- solve(x, ...)
    x$setInverse(inverse)
    return (inverse)
}
