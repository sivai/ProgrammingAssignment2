## The functions makeCacheMatrix and cacheSolve are used to caclulate
## the inverse of a matrix and will cache the results.


## makeCacheMatrix creates a list object with functions that 
## can be used by cacheSolve to cache the inverse matrix. It encapsulates
## the original matrix passed to the function as well as the cached
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve takes a list returned from makeCacheMatrix
## and calculates the inverse matrix. It will cache the results
## and return the cache result on future calls as long as the matrix
## does not change.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
