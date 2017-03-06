## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve first checks the cache as to whether there is an inverted matrix stored
## yes - the stored matrix is retrieved
## no - the matrix is inverted using the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Obtaining stored data.")
    return(inv)
  }
  dat <- x$get()
  inv <- solve(dat)
  x$setinverse(inv)
  inv
}




