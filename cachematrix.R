## makeCacheMatrix avoids calculating repeatedly the inverse of a matrix
# by storing the result in the parent environment and then retrieving
# if it's necessary

## Returns a list of functions that will handle operations
# over the matrix

makeCacheMatrix <- function(x = matrix(c(1, 0, 0, 1), nrow=2,ncol=2,byrow=TRUE)) {
  # Initial value of cached inverse matrix
  cachedInverse <- NULL
  setMatrix <- function(newMatrix) {
    # Assigns the value of the matrix to a variable in the parent environment
    x <<- newMatrix
    # A new matrix is being created, the inverse matrix is cleared
    inverse <<- NULL
  }
  getMatrix <- function() {
    # Returns the matrix
    x
  }
  setInverse <- function(inverse) {
    # Assigns the value of the matrix to a variable in the parent environment 
    # in order to keep this value for future retrieving
    cachedInverse <<- inverse
  }
  getInverse <- function() {
    # Returns the cached inverse matrix
    cachedInverse
  }
  # Returns the list of functions by element name
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## calculates the inverse of a cached matrix created with 
# makeCacheMatrix. This function assumes that that the matrix is invertible.

cacheSolve <- function(x, ...) {
  # Gets the inverse matrix of the parameter
  inverse <- x$getInverse()
  # If is not NULL the inverse then it has been already calculated, and the function
  # skips the computing and returns the cached inverse matrix.
  if(!is.null(inverse)) {
    message("Inverse matrix already calculated. Retrieving cached data.")
  }
  # If is NULL then inverse is calculated
  else {
    message("Calculating inverse matrix.")
    data <- x$getMatrix()
    # Solves the matrix and returns its inverse
    inverse <- solve(data)
    # Stores inverse in cache
    x$setInverse(inverse)
  }
  # Returns the inverse
  inverse
}
