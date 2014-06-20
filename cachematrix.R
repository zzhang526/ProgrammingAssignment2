
## Calculate the inverse of given matrix.
## Result will be cached and can be used directly next time.

## Caches the inverse of x in x.inverse. 
## Also caches the original matrix in x.oldValue.
makeCacheMatrix <- function(x = matrix()) {
  x.oldValue <- x
  x.inverse <- solve(x)
}

## If x doesn't have cached values or x is changed,
## calculates the inverse and cached it.
## Otherwise return the cached inverse.
cacheSolve <- function(x, ...) {
  if (!exists("x.oldValue") || 
      !exists("x.inverse") || 
      !all.equal(x, x.oldValue)) {
    makeCacheMatrix(x)
  }
  x.inverse
}
