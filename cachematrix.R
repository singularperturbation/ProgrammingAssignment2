## These functions wrap a matrix so that the inverse
## is cached.  makeCacheMatrix is the wrapper, while
## cacheSolve computes the inverse (if needed) and
## updates the cache.

## Returns wrapped matrix object and defines setter/getter functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  get <- function() x
  set <- function(y){
    x   <<- y
    inv <<- NULL
  }
  
  getInverse <- function(){
    return(inv)
  }
  
  setInverse <- function(ivinput){
    inv <<- ivinput
  }
  return(list(set=set,get=get,getInverse=getInverse,
              setInverse=setInverse))    
}


## Compute inverse for matrix x, or retrieve from cache
## if it is available.

cacheSolve <- function(x, ...) {
  tempInv <- x$getInverse()
  if (!is.null(tempInv)){
    return(tempInv)
  }
  tempMatrix <- x$get()
  tempInv    <- solve(tempMatrix)
  x$setInverse(tempInv)
  return(tempInv)
}
