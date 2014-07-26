## Caching the inverse of a matrix


## `makeCacheMatrix` takes a matrix as argument. It creates a list containing a 
## function to
## 1.  set the value of a matrix
## 2.  get the value of a matrix
## 3.  set the value of the inverse of a matrix
## 4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x

  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## `cacheSolve` calculates the inverse of the special "matrix" created with the 
## above function. It checks first if a cache copy of the inverse of the matrix exists.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setsolve function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  
  # Get cache value of inverse if it exists
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # Calculate the inverse of a matrix if no cached value exists
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
