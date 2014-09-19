## Below are two functions that are used to create a special
## object that stores a matrix and cache the inverse of the
## matrix.

## "makeCacheMatrix" creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() inv
  
  ## Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## "cacheSolve" computes the inverse of the special "matrix" returned
## by "makeCacheMatrix". It first checks if the inverse has already been
## calculated (and the matrix has not changed), if yes, then the "cachesolve" 
## will retrieve the inverse from the cache and skip the calculations. If 
## not, it calculates the inverse, sets the value in the cache and returns 
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  ## Return the inverse if it has already been calculated
  if (!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse
  inv <- solve(data)
  
  ## Set the inverse to the object
  x$setinverse(inv)
  
  ## Return the inverse of the matrix
  inv
  
}
