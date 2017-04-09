## create a special matrix, calculate its inverse and store it in cache

## Following function is a list of 4 functions i.e. setMatrix, getMatrix, setInverse and
## getInverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(realMatrix) {
    x <<- realMatrix
    i <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverseMatrix) i <<- inverseMatrix
  getInverse <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting inverse from cache")
    return(i)
  }
  matrix <- x$getMatrix()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
}
