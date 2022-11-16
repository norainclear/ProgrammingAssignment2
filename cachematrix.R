## This following function creates a matrix that can cache its inverse, 
## in order to save computation time.

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
  set <- function(y) {
          x <<- y
          c <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) c <<- inverse
  getinverse <- function() c
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
        

## This function computes the inverse of the special "matrix" returned by make CacheMatrix above. If the inverse
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
 c <- x$getinverse()
  if (!is.null(c)) {
          message("getting cached data")
          return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setinverse(c)
  c
 
  ## Return a matrix that is the inverse of 'x'
}
