# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly . 
# The function makeCacheMatrix saves to value of the calculation, which then can 
# be retrieved by the CacheSolve function.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # declare variable 'inv' to store the inverse and assign it to NULL
  inv <- NULL
  
  # set the value of the matrix
  # use the <<- operator to assign a value to an object in an environment 
  # that is different from the current environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # sets the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # gets the value of the inverse of the matrix
  getinverse <- function() inv
  
  # Encapsulate the functions into a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	
}

# This function computes the inverse of a matrix. Assumption: the matrix needs to be
# invertible. If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve retrieves the value from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  # if the inverse has already been calculated, return the cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # there is no cached value, so calculate the inverse and set the value for future retrieval
  data <- x$get()
  inv <- solve(data, ...)
  
  # set the value of the calculation
  x$setinverse(inv)
  
  # return the inverse
  inv
}