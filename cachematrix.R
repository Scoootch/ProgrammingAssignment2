## This pair of functions calculates inverse matrix and uses cached values if it
## can to save some processor time

## The first function, makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the solve (inverse matrix)
## get the value of the solve (inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setSolve <- function(Solve) m <<- Solve
     getSolve <- function() m
     list(set = set, get = get,
          setSolve = setSolve,
          getSolve = getSolve)
}


## The following function calculates the solve (inverse matrix) of the 
## special "matrix" created with the above function. It first checks to see 
## if the solve has already been calculated. If so, it gets the solve 
## from the cache and skips the computation. 
## Otherwise, it calculates the solve of the data and sets the value 
## of the solve in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getSolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setSolve(m)
     m
}
