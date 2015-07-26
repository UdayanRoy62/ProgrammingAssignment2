## Put comments here that give an overall description of what your
## functions do

## x is assumed to be an invertible matrix
## makeCacheMatrix returns: a list of four functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y) {
    x <<- y ## assigns value to an object in an environment outside the function's own environment
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## Test whether the inverse has already been calculated
  ## If yes, use the cached value. Else use solve(); report the inverse, and store it in cache.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
