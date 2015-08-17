## Put comments here that give an overall description of what your
## functions do
# A pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse_matrix <<- inv
  getinverse <- function() inverse_matrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if (!is.null(inv)) {
    message('getting cached')
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinverse(inv)
  inv
}
