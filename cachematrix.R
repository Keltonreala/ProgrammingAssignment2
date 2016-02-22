## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.

## function makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(valueInverse) matrixInverse <<- valueInverse
    getInverse <- function() matrixInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## x is an object of type makeCacheMatric
    ## Return a matrix that is the inverse of the matrix stored in x
    
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        ## the inverse was previously calculated, so just returned the cached value
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## the inverse has not been calculated yet
    
    ## Retrieve the matrix stored in x
    dataMatrix <- x$get()
    
    ## calculates the inverse of the matrix stored in x, cache the value and return it
    matrixInverse = solve(dataMatrix, ...)
    x$setInverse(matrixInverse)
    
    matrixInverse
}
