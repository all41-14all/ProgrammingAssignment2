## The following functions compute the inverse of a square matrix
## and cache the result. If the inverse has previously been 
## calculated, then the inverse will not be calculated, but will
## be retreived from the cache.  The argument must be an invertible 
## square matrix.
###################################################################

## This function creates a special "matrix" object that can cache
## it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix.  If the inverse has already been
## calculated (and the matrix has not changed), then it will 
## retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
