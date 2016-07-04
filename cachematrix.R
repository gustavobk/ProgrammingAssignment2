# This function returns a "special matrix",
# a list contining functions to deal with the list values
# set : set value to matrix
# get : get matrix value
# set_inverse: set value to the inverse matrix
# get_inverse: returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x

  set_inverse <- function(inv_input) inv <<- inv_input
  get_inverse <- function() inv

  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


# Following the example, try to get the inverse matrix
# but check if it exists
# if yes, return the inverse matrix
# if not, gets the matrix, run the function 'solve'
# and sets the value in the cache
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    return(inv)
  }

  inv <- solve( x$get() , ...)
  x$set_inverse(inv)
  inv
}
