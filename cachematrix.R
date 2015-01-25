## Caching the inverse of the matrix

## Creates a set of function which gets and sets the matrix and inverse of it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(n){
    x <<- n
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(mInv){
    inv <<- mInv
  }
  
  getInv <- function() inv
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Retrieve the inverse of matrix if it is cached,  otherwise creates its inverse

cacheSolve <- function(x, ...) {
   inv <- x$getInv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setInv(inv)
  inv
}
