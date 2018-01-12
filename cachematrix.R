## Caching the Inverse of a Matrix
##
## The following functions
## functions do

# Creates a special "matrix" list object that can cache its inverse.
#
# 1 makeCacheMatrix$set - set the value of the matrix
# 2 makeCacheMatrix$get - get the value of the matrix
# 3 makeCacheMatrix$setinverse - set the inverse of the matrix
# 4 makeCacheMatrix$getinverse - get the value of the matrix


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

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.  
## If the inverse has already been calculated, 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
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
