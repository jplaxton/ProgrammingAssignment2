## Two functions written to demonstrate Lexical Scoping in R
## Functions enable caching of computation intensive operation output

## makeCacheMatrix create a storage object for matrix inversions with supporting
## functions for setter and getter behaviours

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
        x <<- y
        matrix_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) matrix_inv <<- inverse
        getinv <- function() matrix_inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## cacheSolve solves for the inverse of a matrix object (defined as object
## makeCacheMatrix).  Function checks if the inverse has already been solved
## and returns the cached version.  If null, executes the solve function to 
## calculate the inverse then caches the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
