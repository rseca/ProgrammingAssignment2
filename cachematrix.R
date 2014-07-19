## The following 2 functions (makeCacheMatrix and cacheSolve) create and invert
## a matrix. If the matrix has already been inverted then the result will be
## retrieved from cache rather than being run again saving time.

## The function makeCacheMatrix creates a matrix object that caches it's inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(cache) m <<- cache
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)     
}

## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve will retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getcache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setcache(m)
    m
}
