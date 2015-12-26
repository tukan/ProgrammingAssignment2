## These functions wrap matrix and implemment memoization for the inverse operation.
## Usage:
##  First, you need to create a wrapped matrix:
##  > wrapped_matrix <- makeCacheMatrix(c(1,2,3,4,5,6), nrow=2, ncol=2))
##  Second, call cacheSolve, to get inverse:
##  > cacheSolve(wrapped_matrix)

## The function creates a wrapper of a matrix object to
## implement caching.
## It returns a list of functions: set, get, setinverse, getinverse.
## $get: returns original matrix object.
## $set: saves original matrix object.
## $getinverse: returns cached inversed matrix object.
## $setinverse: saves inverse of the original matrix object.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calulates inverse of a matrix wrapped by makeCacheMatrix 
## function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
