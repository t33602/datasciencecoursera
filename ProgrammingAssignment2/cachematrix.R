## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix accepts a matrix arguement and builds a list 
## with functions to set and get the matrix and it's inverse. 
## makeCacheMatrix returns this list to the caller.
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  set <- function(y) {
    ## If we haven't computed the inverse before, or if the 
    ## matrix has changed, then save the new matrix and set
    ## the invers value to null
    if (is.null(invMatrix) | !identical(x,y) ) {
       
      x <<- y                 ## Save original
      invMatrix <<- NULL      ## Set inverse to null
    }
 
  }  ## set
  
  get <- function() x
  
  setInverse <- function(z) invMatrix <<- z
  
  getInverse <- function() invMatrix
  
  ## Return a list object with matrix and its functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve takes a list arguement that contains a matrix with its
## associated setter and getter functions.  cacheSolve returns the 
## inverse of the matrix, using a cached copy if available.
cacheSolve <- function(x, ...) {
  
  ## Get the inverse of x.  May be cached value, or may be null
  cacheInverse <- x$getInverse()
  
  ## If the result is null, we will have to compute the inverse
  ## and save the results in cache
  if (is.null(cacheInverse)  ){
     
    ## Compute and store inverse
    cacheInverse <- solve(x$get())
    x$setInverse(cacheInverse)
  } ## if
    
  cacheInverse  ## Return the results
  
}