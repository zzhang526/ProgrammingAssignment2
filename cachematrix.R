
## Calculates the inverse of given matrix.
## Result will be cached and can be used directly next time.

## Makes a 'matrix' object(a list of functions) which can
## be used by cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inverse <<- y
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## If x doesn't have cached values or x is changed,
## calculates the inverse and cached it.
## Otherwise return the cached inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  inverse <- solve(x$get());
  x$setinverse(inverse);
  inverse;
}
