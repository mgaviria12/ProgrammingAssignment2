## The following functions store a matrix object and allow to optimize the calculation of the
## inverse of the matrix since they store in the cache the solution. If the matrix has not
## changed, the is not recalculated but simply retreived from cache.


## makeCacheMatrix returns a list of functions that allow to set and get the matrix. 
## As well as set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve first checks if the matrix has already been inverted and stored in the cache.
## If it has, it retrieves the inverse from cache. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
