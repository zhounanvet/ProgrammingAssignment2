## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly 

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y=matrix()) {
    x <<- y
    xi <<- NULL
  }
  get <- function() as.matrix(x)
  setinverse <- function(xinverse=matrix()) xi <<- xinverse
  getinverse <- function() xi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  matrix <- x$get()

  if (nrow(matrix)==ncol(matrix)) {
      xi <- solve(matrix, ...)
      x$setinverse(xi)
      xi
  }
  else 
    print ('ERROR: x is not a square matrix')
}
