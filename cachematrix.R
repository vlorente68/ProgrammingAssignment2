## These functions take a matrix and cache the matrix and its inverse once
## it is calculated

## makeCacheMatrix receives a matrix as an argument and create a new special
## matrix used to cache the inverse of the matrix received as argument.
##
## The special matrix is really a list of functions:
##      get: return the matrix
##      set: initialize the matrix and set the inverse to NULL
##      getinverse: returns the value cached as inverse matrix
##      setinverse: set and caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(new_inverse) inverse <<- new_inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cachesolve receives the special matrix created with makeCacheMatrix()
## and check if the inverse has already been cached. If so, returs the cached
## value, if not, calculates the inverse using solve(x) function, set the
## cached value and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
