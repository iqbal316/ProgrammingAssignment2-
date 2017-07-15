


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

