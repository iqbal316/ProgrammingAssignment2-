
# ProgrammingAssignmentNo2

## A pair of functions that cashes the inverse of a matrix.

## Creates a "matrix" object that can cache its inverse

makechachematrix <- function(x = matrix()) {  ## set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    ## get the value of the matrix
  setinverse <- function(inverse) m <<- inverse ## set the inverse of the matrix
  getinverse <- function() m  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

-----------------------------------------

## This function computes the inverse of the special "matrix" 
## makechachematrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  
  cachematrix <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting inverse matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }

