## These two funtions are used in combination to calculate and
## cached the inverse of a matrix

## this first function creates a list with 4 different functions
## the first two funtions in the list sets and gets the initial matrix
## the last two functions in the list sets and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_mat) inv <<- inv_mat
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## this function calculates the inverse matrix
## ONLY if it was not calculated before (is not cached)

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
