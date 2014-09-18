## Programming Assignment 2 / Caching the Inverse of a Matrix
## Assignment: write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
  
  ## Returns the matrix
    x
  }
  
  ## Set the inverse of the matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getinverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Return the inverse if its already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from object
  data <- x$get()
  
  ## Calculate the inverse by matrix multiplication
  i <- solve(data)
  
  ## Set the inverse to the object
  x$setinverse(i)
  
  ## Return the matrix
  i
}
