## makeCacheMatrix and cacheSolve work together to optimize the finding of
## the inverse of matrix by caching it the first time it is computed and then
## calling the value of the inverse from the cache rather than recomputing it
## on subsequent calls for the inverse of the same matrix.

## makeCacheMatrix:
## Creates a special "matrix", which is really a list containing a function to
## set the value of the vector, get the value of the vector, set the value of 
## the mean, and get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        # Initially set the inv as NULL
        inv <- NULL
        # setmatrix function sets the matrix
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # getmatrix function gets the matrix
        getmatrix <- function() x
        # setinvserse sets the inverse
        setinverse <- function(inverse) inv <<- inverse
        # getinverse gets the inverse
        getinverse <- function() inv
        # Returns a list with set
        list(setmatrix=setmatrix, 
             getmatrix=getmatrix, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

## cacheSolve:
## The following function calculates the mean of the special matrix created 
## with the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips 
## the computation. Otherwise, it calculates the mean of the data and sets 
## the value of the mean in the cache via the setmean function.
## NOTE: This function assumes that the matrix supplied is always a square 
## invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Call getinverse
        inv <- x$getinverse()
        # See if it has been computed already
        if(!is.null(inv)) {
                # Output that inverse is coming from a cached value
                message("getting cached data")
                # Return the inverse from the cache and exit
                return(inv)
        }
        # Get the matrix
        data <- x$getmatrix()
        # Calculate the inverse
        inv <- solve(data)
        # Set the inverse in the cache
        x$setinverse(inv)
        # Return the computed inverse
        inv
}
