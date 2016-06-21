## These functions provide a means of computing the
## inverse of an invertible matrix and then caching
## the inverse for later use.


## This function creates a matrix object that can hold
## its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv,
         getinv = getinv)
}

## This function either computes the inverse or returns
## the cached inverse if it exists
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
