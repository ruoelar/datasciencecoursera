
## function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse
##
##Creating matrix object:
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
##Making inverse matrix:
get <- function() x
setinverse  <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse is already calculated and the matrix has not changed, cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}

