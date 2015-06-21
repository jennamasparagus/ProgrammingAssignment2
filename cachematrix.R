## caching matrix inversion functions
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) im <<- solve
  getinverse <- function() im
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data.")
    return(im)
  }
  data <- x$get()
  im <- solve(data)
  x$setinverse(im)
  im
}
