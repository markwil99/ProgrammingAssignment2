## Functions "makeCacheMatrix" and "cacheSolve" interact with each other to create a cached
## matrix inversion and call on that cache if its available, and if it is not to invert a square
## matrix.

## "makeCacheMatrix" creates a "special" matrix which is an inversion of a square matrix "x",
## stored in cache environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## "cacheSolve", when calling "makeCacheMatrix", checks to see if the inverse of matrix "x" is
## stored in cache, then returns it if is, or calculates it if isn't.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    }



