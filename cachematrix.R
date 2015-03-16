## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL  #initializes inverse to NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(minv) minv <<- solve
  
  getinv <- function() minv
  
  #the list is returned when the makeCacheMatrix is executed.
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse of the matrix was already done before, the cached inverse is returned. Otherwise,
## the inverse is computed.

cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  #If the inverse was already calculated:
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  #If the inverse was NOT already calculated:
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
