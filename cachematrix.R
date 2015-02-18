## cachematrix.R implements caching the inverse of a matrix rather than 
## compute it repeatedly. 
## Code based on example given by Roger D. Peng on caching the mean
##
## Assume that the matrix supplied is square and invertible

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  setmtrx <- function(y) {
    x <<- y
    invr <<- NULL
  }
  getmtrx <- function() x
  setinvr <- function(ninvr) invr <<- ninvr
  getinvr <- function() invr
  
  list(setmtrx = setmtrx, 
       getmtrx = getmtrx, 
       setinvr = setinvr, 
       getinvr = getinvr)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinvr()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  m<- x$getmtrx()
  im <- solve(m, ...)
  x$setinvr(im)
  im
}
