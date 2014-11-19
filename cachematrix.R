## These functions provide a way to avoid the costly computation of
## a matrix's inverse. If the inverse has already been calculated a
## cached copy of the inverse will be returned instead.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        # the cached inverse
        i <- NULL
        
        # set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL # reset the cached inverse
        }
        
        # get the matrix
        get <- function() x
        
        # set the inverse
        setInverse <- function(inverse) i <<- inverse
        
        # get the inverse
        getInverse <- function() i
        
        # an object providing access to the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix,
## or returns the cached inverse if it has already been calculated.
cacheSolve <- function(x, ...) {
        
        # if the inverse exists, return that
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # otherwise compute the inverse and cache it for next time
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        
        i
}
