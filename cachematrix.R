## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object
  # that can cache its inverse.
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  set.inverse <- function(inv) inve <<- inv
  get.inverse <- function() inve
  return(list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special
  # "matrix" returned by `makeCacheMatrix` function. If the inverse has
  # already been calculated (and the matrix has not changed), then
  # `cacheSolve` should retrieve the inverse from the cache.
  inve <- x$get.inverse()
  if (!is.null(inve)) {
    message('Getting cached inverse')
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$set.inverse(inve)
  return(inve)
}

# Example that shows how to use those two functions
# tu <- matrix(0, nrow = 2, ncol = 2)
# tu[2, 1] <- 2
# tu[1, 2] <- 2
# 
# a <- makeCacheMatrix()
# a$set(tu)
# a$get()
# cacheSolve(a)
# a$get.inverse()

