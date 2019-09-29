## These are 2 functions that cache and return the inverse of a matrix

## This particular matrix creates a matrix object that cashes 
## its own inverse 


makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The following function returns the inverse of this particular matrix
## that is returned by makeCacheMatrix.In the condition that the inverse
## is calculated (without the change inthe matrix), then
## cacheSolve will get the cache inverse.


cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
