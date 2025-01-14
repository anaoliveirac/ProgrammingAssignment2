## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the current matrix
  get <- function() x
  
  # Function to cache the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to retrieve the cached inverse
  getinverse <- function() inv
  
  # Return a list of functions for external access
  list(set = set,  get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check for cached inverse
  
  # If inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute inverse
  x$setinverse(inv)  # Cache the inverse
  inv  # Return the computed inverse
}
