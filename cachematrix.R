##Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
cacheSolve <- function(x,...) {
  ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
