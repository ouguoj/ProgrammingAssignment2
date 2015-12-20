## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function essentially creates a cache that stores the inverse of a matrix, x, and allows the user to get the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
         set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
         message("geting cached data")
         return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse()
        s
}
