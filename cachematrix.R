## makeCacheMatrix creates an object that stores a matrix and its inverse. 
## cacheSolve solves for the inverse of the matrix in the makeCacheMatrix object.

## This function creates an object that stores the state of a matrix as well
## as the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(x) inv <<- x
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## This function solves for the matrix inverse, caches the result and set the inverse value of 
## the makeCacheMatrix object that was passed to it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}
