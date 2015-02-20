## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache a matrix and its inverse.  
## cacheSolve: This function calcuates the inverse of the matrix stored in 
## makeCacheMatrix and caches it.


## Function to cache a matrix and its inverse.  Must take an invertible matrix
## (#rows = #columns) as input.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(z) inv <<- z
  getinv <- function() inv  
  
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## Function to calculate inverse of the matrix created by "makeCacheMatrix."
## If the inverse has already been calculated, and the matrix has not changed,
## the cached inverse is shown instead.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse:")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}