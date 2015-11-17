## This set of functions is used to set and cache a matrix as well
## as calculate, cache and retrieve the inverse via object like function
## structures

## This function is used to store the matrix and inverse of the matrix.
## It has four nested functions which are used to set and get the matrix,
## and inverse matrix.  A list of functions is returned which allows
## the getting and setting of the matrix and inverse matrix. 
makeCacheMatrix <- function(x = matrix()) {
        ## Set our variable which will hold our cached inverse
        ## matrix to NULL.
        m <- NULL
        ## When the matrix is set, set the cached matrix to NULL.
        setmatrix <- function(y) {
            x <<- y
            m <<- NULL
        }
        ## Return the set matrix.
        getmatrix <- function() x
        ## Set the inverse matrix.
        setinvmatrix <- function(invm) m <<-invm
        ## Get the inverse matrix.
        getinvmatrix <- function() m
        ## Return a list with references to the four nested functions.
        list(setmatrix = setmatrix, getmatrix = getmatrix,
               setinvmatrix = setinvmatrix,
               getinvmatrix = getinvmatrix)
}

## This function takes a makeCachedMatrix list (object) and either
## returns the cached inverse matrix, or calculates a new inverse
## matrix, caches it and then return the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Get the value stored in the inverse matrix variable.
        m <- x$getinvmatrix()
        ## Check to see if m is NULL, if not, the inverse matrix
        ## is cached so return the cached matrix.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## m is null, so get the matrix from the makeCacheMatrix 
        ## list.
        data <- x$getmatrix()
        ## Invert the matrix.
        m <-solve(data)
        ## Store the matrix in the makeCacheMatrix list.
        x$setinvmatrix(m)
        ## Return the inverse matrix.
        m
}
