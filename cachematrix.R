
## The following two functions will calculate the inverse of a matrix, the first
## time it is given the matrix.  It will then place the inverse into cache.
## When the matrix is input again, the inverse will be called from cache as opposed
## to recalculating.

## makeCacheMatrix creates a list containing a function to: (1) set the
## value of the matrix, (2) get the value of the matrix, (3) set the inverse
## matrix, and (4) get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    
}


## cachesolve returns the inverse matrix by first checking to see if it has
## already been calculated.  If so, it returns the inverse from the cache,
## otherwise, it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
