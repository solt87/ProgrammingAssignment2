## makeCacheMatrix() creates a special "matrix" object that
## can cache its inverse.
## cacheSolve() returns the inverse of such a special "matrix".


## Creates a "matrix" that can cache its own inverse.
## Returns a list containing functions to view or edit the matrix and its
## inverse.
makeCacheMatrix <- function(x = matrix()) {

        ## Inverse not yet computed
        inv <- NULL
        ## The setter function of the matrix
        set <- function( y ) {
                x <<- y
                ## The matrix just got a new value, so
                ## the inverse is not yet known
                inv <<- NULL
        }
        ## The getter function of the matrix
        get <- function() x
        ## The setter function of the inverse of the matrix
        setinv <- function( invv ) inv <<- invv
        ## The getter function of the inverse of the matrix
        getinv <- function() inv
        ## Return the list that makes the matrix functions accessible
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv )
}


## Returns the inverse of the special matrix 'x', either from
## cache (if available), or by computing it.
## ASSUME that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {

        ## Get the stored inverse (if any) from 'x'
        inv <- x$getinv()
        ## If the inverse is cached, return cached value
        if ( !is.null(inv) ) {
                message("Getting cached data...")
                return( inv )
        } ## Else get the data...
        data <- x$get()
        inv <- solve( data, ... ) ## ... compute its inverse ...
        x$setinv( inv )           ## ... set the inverse in x's cache, and...
        inv                       ## ... return the computed inverse.
}
