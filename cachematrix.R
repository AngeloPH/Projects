## This set of functions creates special "matrix", computes inverse of
## the special "matrix", and retrieves the cached inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) m <<- inv
    get_inv <- function() m
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
