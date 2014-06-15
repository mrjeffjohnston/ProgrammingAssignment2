## A set of functions that create and operate on special matrix
## objects that cache their own inverse.


## Create a new special matrix object that will cache its own inverse.
## The returned object is a list of 4 functions:
##   get        - returns the actual matrix contents
##   set        - set the matrix contents to a new set of values
##   getInverse - returns the currently cached inverse matrix, this will
##                be null if the inverse has not been set yet.
##   setInverse - set the cached inverse of the current matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of the given special matrix object
## If the inverse has been previously calculated then the cached value will be
## returned without recalculating it.
## If there is no cached inverse then the inverse is calculated by calling solve
## and the result stored in the cache.
## The extra parameters are passed through to the solve function if needed.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
