
## The functions makeCacheMatrix and cacheSolve are used
## to cache the inverse of a matrix in order to avoid
## costly repeated computation.

## The function makeCacheMatrix creates a special matrix
## object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##if no function is called sets m (the inverse) to NULL 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##pull up cached matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { ## Return cached inverse matrix that is not empty
    message("getting cached inverse")
    return(m)
  }
  ##pull up cached data, find inverse if cached inverse was empty
  data <- x$get()
  m <-solve(data, ...)
  x$setinv(m)
  m
}
