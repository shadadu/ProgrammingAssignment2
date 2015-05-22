## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {  ## Input matrix x;  

  m <- NULL      ## Default inverse of 'x' is NULL
  set <- function(y) {    ## set is called to give 'x' a user-defined value 'y' 
    x <<- y
    m <<- NULL            ## m is NULL since new matrix 'x' has been defined
  }
  get <- function() x                ## Returns the matrix 'x'
  setInverse <- function(inv) {
    m <<- inv                        ## User-supplied inverse of 'x'; cached
    } 
  getInverse <- function() m         ## Gets the inverse of x
  list(set = set, get = get,         ## Returns a list of four functions for
       setInverse = setInverse,      ## setting and getting matrix 'x' and
       getInverse = getInverse)      ## its inverse
  
  
  
}


## This function computes the inverse of a matrix if the inverse
## of the matrix is not already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {                ## If there is a value cached for inverse 
    message("getting cached data") ## then that value is returned
    return(m)                      ## no need to recompute the inverse
  }
  data <- x$get()
  m <- solve(data, ...)     ## computes the inverse of 'x'
  x$setInverse(m)                  
  m          
  
  
  
}
