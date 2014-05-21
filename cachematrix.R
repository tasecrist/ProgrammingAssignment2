## The functions makeCacheMatrix and cacheSolve are used
## to cache the inverse of a matrix in order to avoid
## costly repeated computation.

## The function makeCacheMatrix creates a special matrix
## object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-matrix(data=NA,nrow(x),ncol(x))
  set<-function(y=matrix()){  
    x<<-y 
    inv<<-matrix(data=NA,nrow(x),ncol(x))
    get<-function() x
    setinverse<-function(inver) inv<<inver
    getinverse<-function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    
  }
  ## The function cacheSolve computes the inverse of the special "matrix"
  ## returned by makeCacheMatrix above.
  ## If the inverse has already been calculated 
  ## (and the matrix has not changed), then cacheSolve 
  ## retrieves the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
    
    inv<-x$getinverse()
    if(!is.na(inv[1,1])){
      message("getting cached inverse")
      return(inv)     ## Return a matrix that is the inverse of 'x'
    }
    matrix<-x$get()
    inver<-solve(matrix) #calculate the inverse of matrix x
    x$setinverse()
    return(inver)   #display the inverse
  }