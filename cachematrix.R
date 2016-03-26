## makeCacheMatrix() creates a special "matrix" object that
## can cache its inverse.
## cacheSolve() returns the inverse of such a special "matrix".


## Creates a "matrix" that can cache its own inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function( y ) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function( invv ) inv <<- invv
        getinv <- function() inv
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv )
}


## Returns the inverse of the special matrix 'x', either from
## cache (if available), or by computing it.
## ASSUME that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if ( !is.null(inv) ) {
                message("Getting cached data...")
                return( inv )
        }
        data <- x$get()
        inv <- solve( data )
        x$setinv( inv )
        inv
}
