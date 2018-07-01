#Programming Assignment 2: Lexical Scoping - Caching the inverse of a matrix
#Sample input and output: 
#> m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
#> m2 <- makeCacheMatrix(m)
#> cacheSolve(m2)
#      [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> cacheSolve(m2)
#inverse is cached
#      [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

#makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #function to get the matrix
  getMatrix <- function() x
  #function to set the matrix
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #getter function for matrix inverse
  getInverse <- function() inverse
  #setter function for matrix inverse
  setInverse <- function(inverse) inverse <<- inverse
  
  list(getMatrix=getMatrix, setMatrix=setMatrix, getInverse=getInverse, setInverse=setInverse)
}


# cacheSolve checks if inverse it cached, if not it computes the inverse and returns it

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  #If inverse is already computed, returns cached matrix
  if (!is.null(inverse)) {
    message("inverse is cached")
    return(inverse)
  }
  
  #compute the inverse usinf solve function
  m <- x$getInverse()
  inverse <- solve(m, ...)
  
  #caches inverse
  x$setInverse(inverse)
  
  #returns the inverse of input matrix
  return(inverse)
}
