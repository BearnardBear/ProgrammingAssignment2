## The following two functions work together to store a matrix and calculate its inverse.
## The purpose is to speed up calculations by storing the matrix's inverse and not calculate
## each time.
##
## Following list of commands is how to use these two functions:
## m <- makeCacheMatrix(cbind(c(2, 2), c(3, 2)))    Create a matrix object
## cacheSolve(m)            Generate and store the inverse of the matrix object
## cacheSolve(m)            Retrieve the stored inverse matrix, without calculating it
##                          
##
## m$set(cbind(c(2, 4, 8), c(4, 6, 5), c(6, 7, 2)))     Change the original matrix        
## cacheSolve(m)            Inverse is recalculated using new matrix and stored 
## cacheSolve(m)            The stored matrix is returned

## This function takes a matrix and stores it in a variable. It has a list of functions
## that are able to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  inv <- NULL
  setinv <- function(y) {
    inv <<- y
  }
  getinv <- function() inv
  get <- function() x
  list(set = set, getinv = getinv, setinv = setinv, get = get)
}


## This function uses the function above to get the inverse of the matrix
## If the matrix has changed or it is the first time running it, it will
## calculate the inverse of the matrix and store that inverse in the function above.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}