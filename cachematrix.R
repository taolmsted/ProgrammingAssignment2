## The following two functions (makeCacheMatrix, cacheSolve) set a matrix into cache and then solve its inverse.

## The makeCacheMatrix fucntion creates a special "matrix" object that can cache its inverse with four separate functions:
##   setMatrix - set the value of a matrix
##   getMatrix - get the value of a matrix
##   setinv = set the value of the inverse of the previous matrix
##   getinv = get the value of the inverse of the previous matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     setMatrix <- function(y) {
          x <<- y
          i <<- NULL
     }
     getMatrix <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     list(setMatrix = setMatrix, getMatrix = getMatrix, setinv = setinv,
          getinv = getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if (!is.null(i)) {
          message("retrieving cached data")
          return(i)     
     }
     m <- x$getMatrix()
     i <- solve(m, ...)
     x$setinv(i)
     i
}
