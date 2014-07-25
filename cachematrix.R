## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
# Calculates the inverse of a matrix created with makeCacheMatrix.
# Only computes the inverse when it is not computed and cached yet.
# It uses the solve function.
# Example:
# m1 <- matrix(c(1,0,0,0,2,0,0,0,3), nrow=3, ncol=3)
# mAux <- makeCacheMatrix(m1)
# m2 <- cacheSolve(mAux)
# round(m1 %*% m2, 3)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
