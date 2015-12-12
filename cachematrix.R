## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly

## The following functions provides a cache for inversion
## function (solve())

# makeCacheMatrix: return a list of functions needed
# for the cache

makeCacheMatrix <- function(x = matrix()) {
  #inv is the inverse
  inv <- NULL
  # 1.- Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 2.- Get the value of the matrix
  get <- function() x
  # 3.- Set the value of the inverse
  setsolve <- function(solve) inv <<- solve
  # 4.- Get the value of the inverse
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: It obtains the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
