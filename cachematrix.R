## makeCacheMatrix and cacheSolve allow you to create a special object
## that stores a matrix, calculate it's inverse, and cache the inverse matrix.


## Sets the value of matrix, gets matrix, sets the inverse matrix, 
## gets the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { 
        x <<- y
        M <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse matrix if the inverse matrix is not cached.
## If the inverse matrix has been cached, the function skips the inverse matrix
## calculation and pulls the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
