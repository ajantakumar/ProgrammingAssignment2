## This program contains two functions:  makeCacheMatrix and 
## and cacheSolve.  

## The makeCacheMatrix function will create a 
## group of functions to store and retrieve a matrix and
## its inverse in the cache.


makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse object.  The variable x is 
  # initalized in the function call.
  inverse <- NULL
  
  # The set function is the "setter" for the matrix in the cache.
  # This function is called to store a matrix different from 
  # the one initially passed to the object
  set <- function(y) {
    x <<- y
    # Re-initialize the inverse because there is a new matrix
    inverse <<- NULL
  }
  
  # The get function is the "getter" for the matrix 
  # stored in the cache.
  get <- function() x
  
  # The setInverse function is the "setter" for the inverse
  # of the matrix in the cache.
  setInverse <- function(inv_matrix) inverse <<- inv_matrix
  
  # The getInverse function is the "getter" for the inverse
  # matrix stored in the cache.
  getInverse <- function() inverse
  
  # The following list command assigns each function in 
  # this object as an element within a list.  The list
  # items are named for easier access.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The cacheSolve function will return the inverse of a matrix
## If the inverse has already been calculated, the function will
## retrieve the inverse from the cache.  Otherwise,
## the inverse will be calculated and stored in the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve the inverse matrix stored in the cache
  inverse <- x$getInverse()
  
  ## If the inverse is not NULL, print a message that 
  ## cached data is being retrieved, print the inverse
  ## and break out of the function
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Retrieve the input matrix from the cache
  matrix_data <- x$get()
  
  ## Calculate the inverse
  inverse <- solve(matrix_data)
  
  ## Store the inverse matrix in the cache
  x$setInverse(inverse)
  
  ## Print the inverse to the console
  inverse
}
