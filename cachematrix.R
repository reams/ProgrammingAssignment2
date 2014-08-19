# Matrix inversion is usually a costly computation
# and there may be some benefit to caching the inverse
# of a matrix rather than computing it repeatedly.
# We are assuming that the matrix supplied
# is always invertible.
#
# The following function creates a special "matrix" object
# that can cache its inverse. It also creates
# a list containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initializes the inverse
  set <- function(y) { # initializes the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # returns the matrix to be inverted
  setinv <- function(inv) m <<- inv # caches the inverse
  getinv <- function() m # returns the cached inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # returns the list of functions
}
#
# The following function computes the inverse of
# the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated
# (and the matrix has not changed), then the following
# function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv() # we get the contents of the cache
  if(!is.null(m)) {# if the cache contains the inverse of x
    message("getting cached data")
    return(m) # we return the contents of the cache
  } # if we get to this point, the cache is empty
  data <- x$get() # so we use get to provide the matrix
  m <- solve(data, ...) # we invert the matrix
  x$setinv(m) # we put the inverse in the cache
  m # we return the inverse
}
#