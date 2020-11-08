## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a list of functions to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of inverse matrix
## d. get the value of the inverse matrix
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


## Write a short comment describing this function
## find the inverse of a matrix
## from cache if present. 
## if not, calculate inverse and save to cache
## for future 'solves'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data, ...)
  x$setinverse(invers)
  invers
}
