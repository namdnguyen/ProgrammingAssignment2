## After first checking if the inverse for the matrix has not already been
## calculated and cached, the cacheSolve() function will calculate the inverse
## of a matrix created with the makeCacheMatrix() function and cache the value.
## If cacheSolve() is called again for the same matrix, then the cached inverse
## value will be returned

## makeCacheMatrix() creates a special matrix that is a list storing the
## matrix, its cached inverse, and functions to set and get its inverse.

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


## cacheSolve() checks for a cached inverse of the matrix created with
## makeCacheMatrix() and returns it if stored. Else, cacheSolve() calculates the
## inverse of the matrix and saves the value in the special matrix.

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
