## This pair of functions uses lexical scoping to save computation time
##  on the inverses of matrices.

## This function creates a list of functions to set and access the data of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # A variable for the inverse
    i <- NULL
    # Set the value of a matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Get the matrix
    get <- function() x
    # Set the inverse of the matrix
    setinv <- function(inv) i <<- inv
    # Get the inverse
    getinv <- function() i
    # Return the list of all 4 functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function returns the inverse of a matrix,
##  or computes the matrix's inverse if it's still NULL

cacheSolve <- function(x, ...) {
    # Get the inverse of the matrix passed in
    i <- x$getinv()
    # If that inverse ins't null, return it
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    # If the inverse is null, solve for it then set it
    m <- x$get()
    i <- solve(m)
    x$setinv(i)
    i
}
