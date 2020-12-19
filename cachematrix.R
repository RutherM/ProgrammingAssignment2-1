## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix contains functions that:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## functions to be used with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
  #initially gives 'i' a 'NULL' value
  i <- NULL 
  
  #set function gives the matrix 'x' the same value as the matrix 'y' 
  set <- function(y) { 
    x <<- y 
    i <<- NULL
  }
  
  #get function returns the matrix 'x' from set
  get <- function() x 
  
  #setinverse inputs the cached inverse matrix 'inverse'
  setinverse <- function(inverse) i <<- inverse 
  
  #getinverse return's the matrix 'i' from setinverse
  getinverse <- function() i 
  
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.cacheSolve returns a matrix 
## that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  #'i' gets the inputted 'inverse'
  i <- x$getinverse() 
  
  #check if the inverse matrix is already determined computation is skipped
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  
  #if the inverse matrix is not yet determined, inverse of matrix computed 
  #using the function solve()
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
