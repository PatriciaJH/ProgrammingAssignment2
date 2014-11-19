## Class exercise: a numeric matrix that caches its inverse during its lifetime.

## function makeCacheMatrix(x)
##
## Constructor function: passed a numeric matrix, return an list-object which can cache its inverse
## 
## m <- matrix(c(4,3,3,2), 2, 2, byrow=TRUE)
##                               # Create the caching matrix:
## cm <- makeCacheMatrix(m)
##                               # Get the value of the matrix:
## cm.get()                      
##                               # Calculate and cache the inverse.
## inv <- cacheSolve(cm)

makeCacheMatrix <- function(x = matrix()) {
    if (!is.numeric(x))
        mode(x) <- "numeric"            # Force a numeric matrix. This could print an error instead...
    inverse <- NULL
    
    set <- function(y) {
        if (!is.numeric(y))
            mode(y) <- "numeric"        # Force a numeric matrix. Could also print an error
        x <<- y                         # <<- searches parent enviroments for an existing variable; else defines it in the global env.
        inverse <<- NULL                # see http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html for more on <<-
    }
    get <- function() x
    setinverse <- function(inv) inverse  <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,          # Return a list of function definitions, with variables x and inverse defined & set in their local environment.
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve(x, ...)
##
## Passed a cacheing matrix, return its inverse, calculating and cacheing the inverse if not yet cached.
## 
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)
##      [,1] [,2]
## [1,]   NA   NA
## [2,]   NA   NA
##

cacheSolve <- function(x, ...) {

    inverse <- x$getinverse()           #  Return a matrix that is the inverse of 'x'

    if (! is.null(inverse)) {           # if cached, just return it.
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()                     # inverse isn't yet calculated.
    inverse <- solve(data, ...)         # calculate inverse;
    x$setinverse(inverse)               # cache it;
    inverse                             # return it.
}
