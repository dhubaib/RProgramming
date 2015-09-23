## This function will compute an inverse for a specified matrix if it has not been
## computed before. Otherwise, it will retreive a cached version of the inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to null
  i <- NULL
  
  # Set function clears inverse when original matrix changes
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  # Define function responsible for storing the inverse
  setinv <- function(solved) i <<- solved
  getinv <- function() i
  
  # Define output as list of functions with global names = local names
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  ## If i is not null, retrieve cached data set
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Otherwise... get the matrix value & compute inverse
  data <- x$get()
  i <- solve(data, ...)
  ## Then cache the inverse
  x$setinv(i)
  i
}
