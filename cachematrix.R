## The functions makeCacheMatrix and cacheSolve create a matrix object 
## capable of caching its inverse, caclulate the inverse, and, if the 
## inverse for the matrix object has already been calcualted, retrieve 
## the inverse from the cache.

## makeCacheMatrix is the function that creates the matrix object capable
## of caching its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is the function that calculates the inverse of a matrix
## object created by makeCacheMatrix, or, if the inverse has already
## been calculated and the matrix object did not change, retrieves the
## inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        mtrx <- x$get()
        i <- solve(mtrx, ...)
        x$setinverse(i)
        i
}

