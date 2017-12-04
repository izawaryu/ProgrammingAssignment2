## For a Coursera assignment, these two functions
## create a matrix object with some way to cache its inverse, and 
## retrieve an inverse if it has already been calculated, or 
## calculates the inverse if it hasn't already been calculated.
## The purpose is to show how lengthy computations can be cached.

## This function creates a matrix object that caches its own invers

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                inv <<- y
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function either;
## 1. Retrieves a cached copy of the inverse matrix if one exists, or
## 2. Calculates the inverse of a CacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
}
