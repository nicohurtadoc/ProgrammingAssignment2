## These functions computes the inverse of an invertible matrix and save both of them in the cache and return it
## in case that it had not already been computed, otherwise it only retrieves the inverse

## This function calls for a vector of functions to apply in case you need the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)      ## generate the vector of functions
}


## This function returns the inverse of a matrix retrieved from the cache and, in case it is not computed, it does this for  

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m        ## Return a matrix that is the inverse of 'x'
}
