
## Assignment 2  
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## set the value of the matrix and get the value of the matrix 
## set the value of the solve and get the value of the solve 


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## The following function calculates the solve of the matrix created with the above function. 
## It first checks to see if the solve has already been calculated. 
## If so, it gets the solve from the cache and skips the computation. 
## Otherwise, it calculates the solve of the data and sets the value of the solve in the cache via the setsolve function 


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
