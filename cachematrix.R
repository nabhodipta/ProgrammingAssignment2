## In this R script file I have written two functions, as part of 
## assignment 2, which craetes a special matrix object with the 
## ability to store its inverse in a cache, and deliver the same 
## on demand without recalculation until the orginal matrix 
## changes, in which case, the inverse is recalculated. This saves
## costly and redundant computation 

## ---------------------------------------------------------------
## This function creates a special "matrix" object that can cache
## its inverse, along with setting and getting its matrix value.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(another_x) {
    x <<- another_x
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv_matrix) inv_x <<- inv_matrix
  getinv <- function() inv_x
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## --------------------------------------------------------------
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If inverse has already been
## calculated (and matrix has not changed), then the cacheSolve 
## would retrieve the inverse from the cache, otherwise it will
## again calculate and store the inverse and also return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message ("retrieving the inverse from the cache")
    return (inv)
  }
  mtx_data <- x$get()
  inv <- solve(mtx_data)
  x$setinv(inv)
  inv
}
