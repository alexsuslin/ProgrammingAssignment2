# Functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  
  get <- function() x
  
  setinversed <- function(ix) inversedMatrix <<- ix
  
  getinversed <- function() inversedMatrix
  
  list(set = set, get = get, 
       setinversed = setinversed,
       getinversed = getinversed)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getinversed()
  
  if(!is.null(inversedMatrix)){
    message("getting cached data")
    return(inversedMatrix)
  }
  
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setinversed(inversedMatrix)
  
  inversedMatrix
}
