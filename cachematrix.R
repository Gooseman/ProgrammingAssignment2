## These functions construct a set of functions for storing a matrix and
## calculating the mean of that matrix.  The matrix and the mean, when
## requested, are stored in the global environment or whichever parent
## the free variables inverseMatrix and x (defined in the set function defined
## in makeCacheMatrix) are found.

## This function creates a list of functions for getting and setting a matrix
## and its mean, usually in the global environment, effetively creating a
## cache.  There is an initial matrix which defaults to empty with a NULL
## inverse.  The set function can be used to change the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL

    set <- function(y)
    {
        x <<- y
        inverseMatrix <<- NULL
    }

    get <- function() x
    setinverse <- function(inv) inverseMatrix <<- inv
    getinverse <- function() inverseMatrix

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The argument to this function is a collection of functions for retrieving
## and storing a matrix and its inverse in a global "cache".  The matrix
## inverse is only calculated if it's not already in the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinverse()

    if(!is.null(inverseMatrix))
    {
        message("getting cached data")
        return(inverseMatrix)
    }

    data <- x$get()
    inverseMatrix <- invertMatrix(data)
    x$setinverse(inverseMatrix)
    inverseMatrix
}

# The inverse of a matrix is calculable if the matrix is square and has a non-
# zero determinant.
invertMatrix <- function(m) {
    inverseMatrix <- NULL
    isSquare <- nrow(m) == ncol(m)
    mdeterminant <- det(m)
    hasNonZeroDeterminant <- (0 != mdeterminant)

    if (isSquare && hasNonZeroDeterminant) {
        inverseMatrix <- solve(m)
    }

    inverseMatrix
}
