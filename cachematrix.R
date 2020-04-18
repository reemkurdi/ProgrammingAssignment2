## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    ## cached values in the beginning are NULL
    cache <- NULL

    ## set matrix in working environment
    set <- function(y) {
      ## use `<<-` to assign a value to an object in different environment
      ## from the current one.
      x <<- y
      cache <<- NULL
    }
    ## get value of matrix
    get <- function() x

    ## invert matrix and store in cache
    setInverse <- function(inverse) cache <<- inverse
    ## get inverted matrix
    getInverse <- function() cache

    ## functions are returned to working environment
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve computes inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()

        ## Checks whether the inverse has already been calculated or not
        ## If it has, and matrix has not changed then cacheSolve retrieves
        ## inverse from the cache
        if(!is.null(cache)){
            message("retrieving cached data")
            return(cache)
        }
        ## Otherwise, create a matrix
        data <- x$get()
        ## if X is a square invertible matrix, then solve(X) returns its inverse
        cache <- solve(data, ...)
        x$setInverse(cache)
        cache
}
