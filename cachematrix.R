## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## We need to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## Compute and cache the inverse of a matrix by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'
## If the inverse matrix has already been done, then cacheSolve should give the inverse from the cache.

cacheSolve <- function(x, ...) {  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
