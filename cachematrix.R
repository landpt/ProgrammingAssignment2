## This assignment is comprised by two functions. Overall, they are used to create and 
## cache the inverse of a matrix. 

## Matrix inversion is generally a costly computation technique - sometimes it consumes a lot of time. 
## These functions are a way to reduce this computational work and wasted time, by caching the pre-calculated 
## inverse matrix into a matrix.

## The first function returns a matrix wrapper that will be used in the second function.

makeCacheMatrix <- function(x = matrix()) {
  
  ## init variables
  
  inv <- NULL
  
  ## set the matrix 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix
  
  get <- function() x
  
  ## set the inverse of the matrix
  
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the inverse of the matrix 
  
  getinverse <- function() inv
  
  ## list of the functions to set/get matrixes
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The second function caches the inverse of the matrix if it is available, returned by makeCacheMatrix function.


cacheSolve <- function(x, ...) {
  
  ## get the inverse
  
  inv <- x$getinverse()
  
  ## get the cached data if it is available
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## get inverse matrix
  
  data <- x$get()
  
  ## calculate the inverse
  
  inv <- solve(data)
  
  ## set in the cached matrix 
  
  x$setinverse(inv)
  
  ## return inverse matrix
  
  inv
}