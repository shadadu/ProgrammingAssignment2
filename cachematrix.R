## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,         ## Returns a list of four functions for
       setInverse = setInverse,      ## setting and getting the matrix and
       getInverse = getInverse)      ## its inverse
  
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  
  
}
