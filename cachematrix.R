## a pair of functions that cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a special "vector", which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  setmatrix <- function(mat) {
    matrix <<- mat;
    inverse <<- NULL;
  }
  getmatrix <- function() return(matrix);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return(list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getinverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  matrix <- cacheMatrix$getmatrix()
  inverse <- solve(matrix, ...)
  cacheMatrix$setinverse(inverse)
  inverse
}
